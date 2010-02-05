;; attach the camera and run this before using this driver:
;; insmod /home/martin/src/andor/andor/src/driver/andordrvlx.ko 
;; sh /home/martin/src/andor/andor/script/andordrvlx_load

(defpackage :andor
  (:use :cl :sb-alien :sb-c-call)
  (:export init
	   request-and-wait
	   copy-data
	   exit
	   get-acquired-data
	   *w*
	   *h*
	   *im*))

(in-package :andor)

(load-shared-object "libandor.so")

(define-alien-routine ("GetAvailableCameras" get-available-cameras)
    unsigned-int
  (total-cameras int :out))

(define-alien-routine ("GetCameraHandle" get-camera-handle)
    unsigned-int
  (camera-index int)
  (camera-handle int :out))

(define-alien-routine ("SetCurrentCamera" set-current-camera)
    unsigned-int
  (camera-handle int))

(define-alien-routine ("Initialize" initialize)
    unsigned-int
  (dir c-string :in))

(define-alien-routine ("SetReadMode" set-read-mode)
    unsigned-int
  (mode int))

(define-alien-routine ("SetExposureTime" set-exposure-time)
    unsigned-int
  (time float))

(define-alien-routine ("GetDetector" get-detector)
    unsigned-int
  (xpixels int :out)
  (ypixels int :out))

(define-alien-routine ("SetShutter" set-shutter)
    unsigned-int
  (typ int)
  (mode int)
  (closing-time int)
  (opening-time int))

(define-alien-routine ("SetImage" set-image)
    unsigned-int
  (hbin int)
  (vbin int)
  (hstart int)
  (hend int)
  (vstart int)
  (vend int))

(define-alien-routine ("StartAcquisition" start-acquisition)
    unsigned-int)

(define-alien-routine ("AbortAcquisition" abort-acquisition)
    unsigned-int)

(define-alien-routine ("GetStatus" get-status)
    unsigned-int
  (status int :out))

(define-alien-routine ("ShutDown" shutdown)
    unsigned-int)

(defconstant drv-acquiring 20072)
(defconstant drv-idle 20073)
(defconstant drv-tempcycle 20074)
(defconstant drv-not-initialized 20075)

(define-alien-routine ("GetAcquiredData" get-acquired-data)
    unsigned-int
  (arr (* int))
  (size unsigned-int))

(define-alien-routine ("GetTotalNumberImagesAcquired" get-total-number-images-acquired)
    unsigned-int
  (index int :out))
(define-alien-routine ("GetSizeOfCircularBuffer" get-size-of-circular-buffer)
    unsigned-int
  (index int :out))
(define-alien-routine ("GetNumberNewImages" get-number-new-images)
    unsigned-int
  (first int :out)
  (last int :out))
(define-alien-routine ("GetOldestImage" get-oldest-image)
    unsigned-int
  (arr (* int))
  (size unsigned-int))
(define-alien-routine ("GetOldestImage16" get-oldest-image16)
    unsigned-int
  (arr (* unsigned-short))
  (size unsigned-int))
(define-alien-routine ("GetMostRecentImage" get-most-recent-image)
    unsigned-int
  (arr (* int))
  (size unsigned-int))
(define-alien-routine ("GetMostRecentImage16" get-most-recent-image16)
    unsigned-int
  (arr (* unsigned-short))
  (size unsigned-int))

;; returns 4 values
(define-alien-routine ("GetAcquisitionTimings" get-acquisition-timings)
    unsigned-int
  (exposure float :out)
  (accumulate float :out)
  (kinetic float :out))

(define-alien-routine ("SetKineticCycleTime" set-kinetic-cycle-time)
    unsigned-int
  (time float))

(define-alien-routine ("SetAcquisitionMode" set-acquisition-mode)
    unsigned-int
  (mode int))

(define-alien-routine ("GetNumberHSSpeeds" get-number-hs-speeds)
    unsigned-int
  (channel int)
  (typ int)
  (speeds int :out))

(define-alien-routine ("GetNumberVSSpeeds" get-number-vs-speeds)
    unsigned-int
  (speeds int :out))

(define-alien-routine ("GetNumberADChannels" get-number-ad-channels)
    unsigned-int
  (channels int :out))

(define-alien-routine ("GetNumberAmp" get-number-amp)
    unsigned-int
  (amp int :out))

(define-alien-routine ("GetNumberPreAmpGains" get-number-pre-amp-gains)
    unsigned-int
  (amp int :out))

(define-alien-routine ("SetADChannel" set-ad-channel)
    unsigned-int
  (channel int))

(define-alien-routine ("SetHSSpeed" set-hs-speed)
    unsigned-int
  (typ int)
  (index int))



;; either use get-number-new-images and get-images or use
;; get-most-recent-image (or get-oldest-image)

(defparameter *w* 0)
(defparameter *h* 0)

(defun init (&key (hbin 1) (vbin 1) (hstart 1) (vstart 1) (w 1392 w-p) (h 1040 h-p) (exposure-s 0.01))
  (multiple-value-bind (ret2 handle)
      (multiple-value-bind (ret cams)
	  (get-available-cameras)
	(declare (ignore ret))
	(get-camera-handle (1- cams)))
    (declare (ignore ret2))
    (set-current-camera handle))
  
  
  (initialize "/usr/local/etc/andor")
  (set-read-mode 4)

  (set-exposure-time exposure-s)
  
  (multiple-value-bind (ret w2 h2)
      (get-detector)
    (declare (ignore ret))
    (set-shutter 1 0 50 50)
    (set-image hbin vbin hstart (if w-p w w2) vstart (if h-p h h2))
    (setf *w* w
	  *h* h)))


(defun request-and-wait ()
  (start-acquisition)
  (loop while (= drv-acquiring
		 (multiple-value-bind (a b)
		     (get-status)
		   (declare (ignore a))
		   b))
    do (sleep .005)))

(defparameter *im* nil)

(defun copy-data ()
  (setf *im*
	(let* ((img (make-array (list *w* *h*) :element-type '(signed-byte 32)))
	       (img1 (sb-ext:array-storage-vector img)))
	  (sb-sys:with-pinned-objects (img1)
	    (get-acquired-data (sb-sys:vector-sap img1)
			       (* *w* *h*)))
	  img)))

(defun exit ()
 (shutdown))

#+nil
(init)

#+nil
(request-and-wait)

#+nil
(copy-data)

#+nil
(exit)

(defun init-run-till-abort
    (&key (hbin 1) (vbin 1) (hstart 1) (vstart 1)
     (w 1392 w-p) (h 1040 h-p) (exposure-s 0.01))
  (multiple-value-bind (ret2 handle)
      (multiple-value-bind (ret cams)
	  (get-available-cameras)
	(declare (ignore ret))
	(get-camera-handle (1- cams)))
    (declare (ignore ret2))
    (set-current-camera handle))
  
  
  (initialize "/usr/local/etc/andor")
  (set-read-mode 4)
  (set-kinetic-cycle-time 0f0)
  (set-exposure-time exposure-s)
  (set-ad-channel 0)
  (set-hs-speed 1 0)
  (multiple-value-bind (ret w2 h2)
      (get-detector)
    (declare (ignore ret))
    (set-shutter 1 0 50 50)
    (set-image hbin vbin hstart (if w-p w w2) vstart (if h-p h h2))
    (setf *w* w
	  *h* h))
  
  (multiple-value-bind (ret exp acc kin)
      (get-acquisition-timings)
    (declare (ignore ret exp acc))
    kin))

#+nil
(time
 (init-run-till-abort))

#+nil
(progn
  (start-acquisition)
  )

;; (get-number-vs-speeds)
;; (get-number-hs-speeds 1 1)
;; (get-number-ad-channels)
;; (get-number-amp)
;; (get-number-pre-amp-gains)