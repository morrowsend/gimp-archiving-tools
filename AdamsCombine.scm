; Filename: AdamsCombine.scm
; By Adam Harris http://sheekgeek.com  Jan. 2013 
; Loosely based on the jmat-layout2_0.2.scm Raymond Ostertag <r.ostertag@caramail.com>
; which is in turn based on version 0.1 by James Waldby - j-waldby@pat7.com - 
; 30 March 2003 http://gimpfr.org/pub/scripts-2.2/yahoo_groupe/jmat-layout2_0.2.scm
;  
; Description:
; Version 0.3 - 
; Scripts for combining images side-by-side without scaling,
; The resulting image can be automatically saved or can be 
; opened for veiwing to be saved manually. Filetype is also selectable.
; The height of the resulting image is equal to that of the tallest image.


;====================================================================
; get-copy copies scaled top layer of image input Image to a new layer 
; in image imo.
; Parameters:  imageA = image in, to be copied.
; imageOut = image out, to be copied to.
; xoff = x offset of new layer position in imageOut,
; yoff = y offset of new layer position in imageOut,
(define (script-fu-get-copy imageIn imageOut widthNew heightNew xoff yoff)
  (let* (
	 (layerIn (car (gimp-image-get-active-layer imageIn)))
	 (layerOut (car (gimp-layer-new imageOut widthNew heightNew RGB-IMAGE "c" 100 0)))
	)
    (gimp-image-add-layer imageOut layerOut 0)	; Add a layer to new image
    (gimp-selection-all imageIn)
    (gimp-edit-copy layerIn)
    (let ((t (car (gimp-edit-paste layerOut 0)))
	 )
      (gimp-layer-set-offsets t 0 0)	; avoid an offset problem
      (gimp-floating-sel-anchor t)
     )
    (gimp-layer-set-offsets layerOut xoff yoff) ; Offset the layer
   )
) ; end script-fu-get-copy




;====================================================================
; get-width computes aspect ratio of active layer of image
; Parameters:  image = image in, to be copied.
(define (script-fu-get-width image)
  (let* (
	 (Buff (car (gimp-image-get-active-layer image)))
	 (width (car (gimp-drawable-width Buff)))
	)
    width
   )
) ; end script-fu-get-width



;====================================================================
; get-width computes aspect ratio of active layer of image
; Parameters:  image = image in, to be copied.
(define (script-fu-get-height image)
  (let* (
	 (Buff (car (gimp-image-get-active-layer image)))
	 (height (car (gimp-drawable-height Buff)))
	)
    height
   )
) ; end script-fu-get-height




;====================================================================
; Given 2 images, combine them into one row, with a height of the tallest image and width of both images added
(define (script-fu-Adam-combine active-image active-drawable imageA imageB inSaveFile closeFile inDir inSaveType outFileName)
; active-img and active-drawable are not used, they exist just to make sure the image selection boxes appear correctly.
; reference: http://registry.gimp.org/node/24404
    (let*(
	; get specs of image a
         (widthA (script-fu-get-width imageA))
	 (heightA (script-fu-get-height imageA))
	; get specs of image b
         (widthB (script-fu-get-width imageB))
	 (heightB (script-fu-get-height imageB))  ; set width of output image = to width of both input images added	  
	 (width (+ widthA widthB))
         (height 0)	; make variable empty for now       
	 (newImage 0)
         (allNewLayers 0 )
         (saveString "")
         (newFileName "")
         (pathchar (if (equal? (substring gimp-dir 0 1) "/") "/" "\\")) ;make sure directory slashes are the right direction.
	); end of declaring vars

; ; set height of new image to that of highest input image
   (if (<=  heightA heightB)	; if height A >= height B, use that height for the new image
       (set! height (* 1 heightA))  ; use height A
       (set! height (* 1 heightB))  ;else use height B
    ); end if
    
    (set! newImage (car (gimp-image-new width height 0))) ; make new RGB image
    (gimp-image-undo-disable newImage)
    (script-fu-get-copy imageA newImage widthA height 0  0) ; Copy ImageA to top left
    (script-fu-get-copy imageB newImage widthB height widthA 0) ; Copy Image to line up exactly next to imageA (top center)
    (gimp-image-undo-enable newImage)		; Re-enable undo
    (gimp-display-new newImage)		; Show the new image


  ;set up saving
    (if (= inSaveFile TRUE)
      (begin
      (set! saveString
      (cond 
        (( equal? inSaveType 0 ) ".tiff" )
	(( equal? inSaveType 1 ) ".jpg" )
        (( equal? inSaveType 2 ) ".bmp" )
        (( equal? inSaveType 3 ) ".png" )
      ); end cond
    ); end set! saveString

          ;save file
      (set! newFileName (string-append inDir pathchar outFileName saveString))
;      (gimp-message newFileName)  ;used in debugging
      (set! allNewLayers (car(gimp-image-flatten newImage)))	; flatten all layers of the new image
      (gimp-file-save RUN-NONINTERACTIVE newImage allNewLayers newFileName newFileName) ; automatically save file
 ); end begin
);end if = inSaveFile = true



    ;done
    (gimp-image-undo-enable newImage)
    (gimp-progress-end)
    (gimp-displays-flush)

    (if (= closeFile TRUE)
      (gimp-display-delete newImage)   ;closes new image
    ); 

;    (gimp-context-pop)
); end biggest let


  )    ;end script-fu-Adam-mat2



;====================================================================
(script-fu-register "script-fu-Adam-combine"
   "<Image>/Filters/Archiving/Adam's Combine Images" ;script location in gimp menu
   "Put two pictures side-by-side in a new picture. There is no scaling.  This script is intended for combining the fronts and backs of photos that have been scanned for archival or other purposes, but can be used to combine any two images side-by-side. The height of the resulting image is equal to that of the tallest image." ;help
   "Adam Harris (of this version)"			;author
   "2013 Adam Harris, based on previous work of James Waldby and Raymond Ostertag 2003"		;copyright
   "28 Janurary 2013"			;date
   ""					;image type that the script works on, all types
   
       SF-IMAGE      "Image"    0	; this is used just to make the image selection boxes appear correctly, the value is not used
       SF-DRAWABLE   "Drawable" 0 ; this is used just to make the image selection boxes appear correctly, the value is not
       SF-IMAGE	     "Left Image"  0
       SF-IMAGE	     "Right Image" 1
       SF-TOGGLE     "Save Extracted Images"     TRUE 
       SF-TOGGLE     "AutoClose Resulting Images"     FALSE 
       SF-DIRNAME    "Save Directory"                      ""
       SF-OPTION     "Save File Type"                      (list "tiff" "jpg" "bmp" "png")
       SF-STRING     "Output Filename"                     "combined_image" 

      
  )
