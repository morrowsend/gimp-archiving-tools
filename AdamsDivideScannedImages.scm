; Filename: AdamsDivideScannedImages.scm
; By: Adam Harris http://www.sheekgeek.org
; Based on DivideScannedImages.scm by Rob Antonishen http://ffaat.pointclark.net
;
; Description:
; Locates each separate element and creates a new image from each.
; will call the deskew plugin http://www.cubewano.org/gimp-deskew-plugin/
; if it is installed on each image
;
; Changes:
; Version 2.0 - Added a couple features to help with archiving photos.
;             - Added "Append to Filename"
;             - Added TIFF to "save file" selections and made this default.
;
; License:
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version. 
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; The GNU Public License is available at
; http://www.gnu.org/copyleft/gpl.html

(define (script_fu_AdamsDivideScannedImages img inLayer inThreshold inSize inLimit inCorner inX inY deSkewImages inSaveFiles closeFile inDir inSaveType inFileName inFileNumber inFileAppend)
  (let*
    (
      (width (car (gimp-image-width img)))
      (height (car (gimp-image-height img)))
      (newpath 0)
      (strokes 0)
      (tempVector 0)
      (tempImage 0)
      (tempLayer 0)
      (bounds 0)
      (count 0)
      (numextracted 0)
      (saveString "")
      (newFileName "")
      (tempdisplay 0)
      (buffname "dsibuff")
      (pathchar (if (equal? (substring gimp-dir 0 1) "/") "/" "\\"))
    )
    ;  it begins here
    (gimp-context-push)
    (gimp-image-undo-disable img)
    
    ;logging
    ;(gimp-message-set-handler ERROR-CONSOLE)
    ;(gimp-message-set-handler CONSOLE)
    ;(gimp-message-set-handler MESSAGE-BOX)
    ;or start GIMP wwith "gimp --console-messages" to spawn a console box
    ;then use this:
    ;(gimp-message "foobar") 



    ;testing for functions defined
    ;(if (defined? 'plug-in-shift) (gimp-message "It Exists") (gimp-message "Doesnt Exist"))

    ;set up saving
    (if (= inSaveFiles TRUE)
      (set! saveString
      (cond 
        (( equal? inSaveType 0 ) ".tiff" )
	(( equal? inSaveType 1 ) ".jpg" )
        (( equal? inSaveType 2 ) ".bmp" )
        (( equal? inSaveType 3 ) ".png" )
      )
    ))
    


    ; convert in inverted copy of the background selection to a path using the selected corner
    (cond ;Number corresponds to the placement in the list below...  I switched Bottom Right to be the first entry.
      ( (equal? inCorner 3) ; 
        (gimp-fuzzy-select inLayer inX inY inThreshold CHANNEL-OP-REPLACE TRUE FALSE 0 TRUE) 
        (gimp-context-set-background (car (gimp-image-pick-color img inLayer inX inY FALSE FALSE 0)))
      )
      ( (equal? inCorner 1)
        (gimp-fuzzy-select inLayer (- width inX) inY inThreshold CHANNEL-OP-REPLACE TRUE FALSE 0 TRUE) 
        (gimp-context-set-background (car (gimp-image-pick-color img inLayer (- width inX) inY FALSE FALSE 0)))
      )
      ( (equal? inCorner 2)
        (gimp-fuzzy-select inLayer inX (- height inY) inThreshold CHANNEL-OP-REPLACE TRUE FALSE 0 TRUE) 
        (gimp-context-set-background (car (gimp-image-pick-color img inLayer inX (- height inY) FALSE FALSE 0)))
      )
      ( (equal? inCorner 0)
        (gimp-fuzzy-select inLayer (- width inX) (- height inY) inThreshold CHANNEL-OP-REPLACE TRUE FALSE 0 TRUE) 
        (gimp-context-set-background (car (gimp-image-pick-color img inLayer (- width inX) (- height inY) FALSE FALSE 0)))
      )
    )
    (gimp-selection-feather img (/ (min width height) 100))
    (gimp-selection-sharpen img)
    (gimp-selection-invert img)
    (plug-in-sel2path RUN-NONINTERACTIVE img inLayer)
    
    ;break up the vectors
    (set! newpath (vector-ref (cadr (gimp-image-get-vectors img)) 0)) 
   
    (set! strokes (gimp-vectors-get-strokes newpath))
    (while (and (< count (car strokes)) (< numextracted inLimit))
    
      (set! tempVector (gimp-vectors-new img "Temp"))
      (gimp-image-add-vectors img (car tempVector) -1)
      (gimp-vectors-stroke-new-from-points (car tempVector)
        (list-ref (gimp-vectors-stroke-get-points newpath (vector-ref (cadr strokes) count)) 0)
        (list-ref (gimp-vectors-stroke-get-points newpath (vector-ref (cadr strokes) count)) 1)
        (list-ref (gimp-vectors-stroke-get-points newpath (vector-ref (cadr strokes) count)) 2)
        (list-ref (gimp-vectors-stroke-get-points newpath (vector-ref (cadr strokes) count)) 3)
      )
      (gimp-vectors-to-selection (car tempVector) CHANNEL-OP-REPLACE TRUE FALSE 0 0)
      
      ;check for minimum size
      (set! bounds (gimp-selection-bounds img))
      (if (and (> (- (list-ref bounds 3) (list-ref bounds 1)) inSize) (> (- (list-ref bounds 4) (list-ref bounds 2)) inSize) ;min size slider
               (< (- (list-ref bounds 3) (list-ref bounds 1)) width) (< (- (list-ref bounds 4) (list-ref bounds 2)) height)) ;max size image
        (begin
          (gimp-rect-select img (list-ref bounds 1) (list-ref bounds 2)
                                (- (list-ref bounds 3) (list-ref bounds 1)) (- (list-ref bounds 4) (list-ref bounds 2))
                                CHANNEL-OP-REPLACE FALSE 0 )
          (set! buffname (car (gimp-edit-named-copy inLayer buffname)))
          (set! tempImage (car (gimp-edit-named-paste-as-new buffname)))
          (set! tempLayer (car (gimp-image-get-active-layer tempImage)))
          (gimp-image-undo-disable tempImage)
          (set! tempdisplay (car (gimp-display-new tempImage)))
      


        (if (= deSkewImages TRUE)
         (begin
            ;run deskew if it is installed
            (if (defined? 'gimp-deskew-plugin) 
                (begin
                (gimp-progress-set-text "Deskewing...")
                (gimp-layer-flatten tempLayer)
                (gimp-deskew-plugin 0 tempImage tempLayer 0 0 0 0 0)
                (gimp-image-resize-to-layers tempImage)
                (gimp-layer-flatten tempLayer)
                (gimp-fuzzy-select tempLayer 0 0 inThreshold CHANNEL-OP-REPLACE TRUE FALSE 0 TRUE) 
                (gimp-selection-invert tempImage)
                (set! bounds (gimp-selection-bounds tempImage))
                (gimp-selection-none tempImage)
                (gimp-image-crop tempImage (- (list-ref bounds 3) (list-ref bounds 1)) (- (list-ref bounds 4) (list-ref bounds 2)) 
                                  (list-ref bounds 1) (list-ref bounds 2))
               )
             )
            )
          )
          (gimp-image-undo-enable tempImage)
          
          ;save file
          (if (= inSaveFiles TRUE)
          (begin
            (set! newFileName (string-append inDir pathchar inFileName 
                                       (substring "00000" (string-length (number->string (+ inFileNumber numextracted)))) 
                                       (number->string (+ inFileNumber numextracted)) inFileAppend saveString))
            (gimp-file-save RUN-NONINTERACTIVE tempImage tempLayer newFileName newFileName)
            
 (if (= closeFile TRUE)
(gimp-display-delete tempdisplay)
); end if closeFile
          ); end if saveFile begin
          ); end if saveFile
          
          (set! numextracted (+ numextracted 1))
        )
      )     
      (gimp-image-remove-vectors img (car tempVector))
      (set! count (+ count 1))
    )

    ;input drawable name should be set to 1919191919 if in batch
    (if (and (> numextracted 0) (equal? (car (gimp-drawable-get-name inLayer)) "1919191919"))
      (gimp-drawable-set-name inLayer (number->string (+ 1919191919 numextracted))))

    ;delete temp path
    (gimp-image-remove-vectors img newpath)
    (gimp-selection-none img)
    
    ;done
    (gimp-image-undo-enable img)
    (gimp-progress-end)
    (gimp-displays-flush)
    (gimp-context-pop)
  )
)

(script-fu-register "script_fu_AdamsDivideScannedImages"
                    "<Image>/Filters/Archiving/Adams Divide Scanned Images..."
                    "Attempts to isolate each part of the image from the background and creates a new image from it"
                    "Rob Antonishen, modified by Adam Harris"
                    "Rob Antonishen, modified by Adam Harris"
                    "Jan 2013"
                    "RGB* GRAY*"
                    SF-IMAGE      "image"      0
                    SF-DRAWABLE   "drawable"   0
                    SF-ADJUSTMENT "Selection Threshold"                 (list 10 0 255 1 10 1 SF-SLIDER)
                    SF-ADJUSTMENT "Size Threshold"                      (list 100 0 2000 10 100 1 SF-SLIDER)        
                    SF-ADJUSTMENT "Abort Limit"                         (list 5 1 100 1 10 1 SF-SLIDER)                         
                    SF-OPTION     "Background Sample Corner"            (list "Bottom Right" "Top Left" "Top Right" "Bottom Left")
                    SF-ADJUSTMENT "Background Sample X Offset"          (list 5 1 100 1 10 1 SF-SLIDER)                         
                    SF-ADJUSTMENT "Background Sample Y Offset"          (list 5 1 100 1 10 1 SF-SLIDER)
                    SF-TOGGLE     "Run Deskew on all the Images"        TRUE       
                    SF-TOGGLE     "Save Extracted Images"               TRUE   
                    SF-TOGGLE     "Close Extracted Images"              TRUE           
                    SF-DIRNAME    "Save Directory"                      ""
                    SF-OPTION     "Save File Type"                      (list "tiff" "jpg" "bmp" "png")
                    SF-STRING     "Save File Base Name"                 "IMAGE"
                    SF-ADJUSTMENT "Save File Start Number"              (list 0 0 9000 1 100 0 SF-SPINNER)
                    SF-STRING     "Append to Filename"                  ""                  
)



;  (let* ((nameparts (strbreakup (car (gimp-image-get-filename image))".")))); end lets*
 ;(gimp-message nameparts) ; prints out filename minus extension
