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
	   init-run-till-abort
	   copy-most-recent-data
	   abort-acquisition
	   start-acquisition
	   wait
	   *w*
	   *h*
	   *im*
	   val3
	   val2
	   get-acquisition-progress))

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

(defconstant drv-success 20002)
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

(define-alien-routine ("GetBitDepth" get-bit-depth)
    unsigned-int
  (channel int)
  (depth int :out))

(define-alien-routine ("GetHSSpeed" get-hs-speed)
    unsigned-int
  (channel int)
  (typ int)
  (index int)
  (speed float :out))

(define-alien-routine ("GetVSSpeed" get-vs-speed)
    unsigned-int
  (index int)
  (speed float :out))

(define-alien-routine ("GetPreAmpGain" get-pre-amp-gain)
    unsigned-int
  (index int)
  (gain float :out))

(define-alien-routine ("IsPreAmpGainAvailable" is-pre-amp-gain-available)
    unsigned-int
  (channel int)
  (amplifier int)
  (index int)
  (pa int)
  (status int :out))

(define-alien-routine ("IsInternalMechanicalShutter" is-internal-mechanical-shutter)
    unsigned-int
  (internal-shutter int :out))
#+nil 
(is-internal-mechanical-shutter)

(define-alien-routine ("GetCapabilities" get-capabilities)
    unsigned-int
  (caps (array unsigned-int 12) :out))

#+nil (loop for i below 12 collect
     (deref (val2 (get-capabilities)) i))

(define-alien-routine ("GetTemperature" get-temperature)
    unsigned-int
  (temp int :out))

(define-alien-routine ("GetTemperatureF" get-temperature-f) ;; detector in degrees celsius
    unsigned-int
  (temp float :out))

#+nil 
(get-temperature-f)

(define-alien-routine ("GetTemperatureRange" get-temperature-range)
    unsigned-int
  (mintemp int :out)
  (maxtemp int :out))

#+nil 
(get-temperature-range)

(define-alien-routine ("GetTemperatureStatus" get-temperature-status)
    unsigned-int
  (sensor-temp float :out)
  (target-temp float :out)
  (ambient-temp float :out)
  (cooler-volts float :out))

#+nil
(get-temperature-status)

(define-alien-routine ("SetTemperature" set-temperature)
    unsigned-int
  (temp int))

#+nil
(set-temperature 0)

(define-alien-routine ("CoolerON" cooler-on)
    unsigned-int)

(define-alien-routine ("CoolerOFF" cooler-off)
    unsigned-int)

(define-alien-routine ("FreeInternalMemory" free-internal-memory)
    unsigned-int)

(define-alien-routine ("IsCoolerOn" is-cooler-on)
    unsigned-int
  (status int :out))

(define-alien-routine ("SetOutputAmplifier" set-output-amplifier)
    unsigned-int
  (typ int))

(define-alien-routine ("SetFrameTransferMode" set-frame-transfer-mode)
    unsigned-int
  (mode int))

(define-alien-routine ("SetTriggerMode" set-trigger-mode)
    unsigned-int
  (mode int))

(define-alien-routine ("GetAcquisitionProgress" get-acquisition-progress)
    unsigned-int
  (acc long :out)  ;; number of accumulation and series scans completed
  (series long :out)) ;; number of kinetic scans completed

#+nil 
(is-cooler-on)

#+nil
(cooler-on)

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


(defun wait ()
  (loop while (= drv-acquiring
		 (multiple-value-bind (a b)
		     (get-status)
		   (declare (ignore a))
		   b))
    do (sleep .005)))

(defun request-and-wait ()
  (start-acquisition)
  (wait)
  )

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

(defmacro val2 (fn)
  `(multiple-value-bind (a b)
       ,fn
     b))

(defmacro val3 (fn)
  `(multiple-value-bind (a b c)
       ,fn
     c))

#+nil
(init) 

#+nil
(request-and-wait)

#+nil
(progn
  (copy-data)
  nil)

#+nil
(aref *im* 5 4)

#+nil
(exit)

#+nil 
(progn
  (format t "~a~%"
	  (list 
	   "number-adc" (val2 (get-number-ad-channels))
	   "number-amp" (val2 (get-number-amp))
	   "number-pre-amp" (val2 (get-number-pre-amp-gains))
	   "number-vs-speed" (val2 (get-number-vs-speeds))
	   "vs-speed" (val2 (get-vs-speed 0))
	   "bit-depths" 
	   (loop for i below (val2 (get-number-ad-channels)) collect
		(arg2 (get-bit-depth i)))
	   "hs-speeds"
	   (loop for chan below (val2 (get-number-ad-channels)) collect
		(loop for amp below (val2 (get-number-amp)) collect
		     (list "chan" chan "amp" amp "nr-speeds"
			   (arg2 (get-number-hs-speeds chan amp))
			   "hs-speeds"
			   (loop for speed below (arg2 (get-number-hs-speeds chan amp))
				collect
				(list 
				 "hs-speed" (val2 (get-hs-speed chan amp speed))
				 "pre-amp-0?" (val2 (is-pre-amp-gain-available chan amp speed 0))
				 "pre-amp-1?" (val2 (is-pre-amp-gain-available chan amp speed 1))
				 "pre-amp-2?" (val2 (is-pre-amp-gain-available chan amp speed 2))))))))))

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
  (set-acquisition-mode 5)
  (set-kinetic-cycle-time 0f0)
  (set-exposure-time exposure-s)
  (set-ad-channel 1)
  (set-output-amplifier 0)
  (set-hs-speed 0 0)
  (set-frame-transfer-mode 1)
  (set-trigger-mode 0)
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
(start-acquisition)
#+nil
(copy-most-recent-data)
#+nil
(aref *im* 0 0)

#+nil
(val3 (get-acquisition-progress))

#+nil
(time 
 (progn
   (start-acquisition)
   (let ((start (val3 (get-acquisition-progress))))
     (dotimes (i 100)
       (loop while (<= (- (val3 (get-acquisition-progress)) start)
		       i) do
	    (sleep .005))
       (copy-most-recent-data)
       (format t "~a~%" (list (aref *im* 0 0) 
			      (val2 (get-total-number-images-acquired))
			      (multiple-value-bind (a b c)
				  (get-number-new-images)
				(list b c))))))))
#+nil
(abort-acquisition)

;; 9.374 s for 100 images

;; (get-number-vs-speeds)
;; (get-number-hs-speeds 1 1)
;; (get-number-ad-channels)
;; (get-number-amp)
;; (get-number-pre-amp-gains)

;; (get-total-number-images-acquired)
;; (get-size-of-circular-buffer)
;; (get-number-new-images)

(defun copy-most-recent-data ()
  (setf *im*
	(let* ((img (make-array (list *w* *h*) :element-type '(signed-byte 32)))
	       (img1 (sb-ext:array-storage-vector img)))
	  (sb-sys:with-pinned-objects (img1)
	    (get-oldest-image (sb-sys:vector-sap img1)
			       (* *w* *h*)))
	  img))
  (free-internal-memory))

#+nil
(progn
  nil)

