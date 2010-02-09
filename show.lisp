#.(progn 
  (require :asdf)
  (require :cl-opengl)
  (require :cl-glut)
  (require :cl-glu))

#.(load "andor.lisp")

(defpackage :show
  (:use :cl :gl))

(in-package :show)

(defclass mk-window (glut:window)
  ()
  (:default-initargs :width 1392 :height 1040
   :pos-y 10 :mode '(:double :rgb)))

(defmethod glut:display-window :before ((w mk-window))
  (point-size 2))

(defparameter *obj* 0)

(defun get-min ()
  (let* ((dat1 (sb-ext:array-storage-vector andor:*im*))
	 (len (array-total-size dat1)))
    (declare (type (simple-array (signed-byte 32) *) dat1))
    (loop for i below len
       minimize (aref dat1 i))))

(defun get-max ()
  (let* ((dat1 (sb-ext:array-storage-vector andor:*im*))
	 (len (array-total-size dat1)))
    (declare (type (simple-array (signed-byte 32) *) dat1))
    (loop for i below len
       maximize (aref dat1 i))))


(defparameter start-series 0)

(defmethod glut:reshape ((win mk-window) w h)
  (setf (glut:width win) w
        (glut:height win) h)
  (format t "reshape ~a~%" (list w h))
  (load-identity)
  (viewport 0 0 w h)
  (matrix-mode :projection)
  (load-identity)
  (ortho 0 w 0 h -1 1)
  (matrix-mode :modelview)
  (load-identity)
  
  (setf *obj* (first (gen-textures 1)))
  (let* ((target :texture-rectangle-nv))
    
    (bind-texture target *obj*)
    (enable target)
    (tex-parameter target :texture-min-filter :linear)
    (tex-parameter target :texture-mag-filter :linear)
    (pixel-store :unpack-alignment 1)
    
    (if andor:*im*
	(let* ((w andor:*w*)
	       (h andor:*h*)
	       (len (* w h))
	       (dat1 (sb-ext:array-storage-vector andor:*im*))
	       (bytes (make-array len :element-type '(unsigned-byte 8)))
	       (max-int (get-max))
	       (min-int (get-min))
	       (scale (/ 255f0 (- max-int min-int))))
	  (declare 
	   (single-float scale)
	   (type (simple-array (unsigned-byte 8) *) bytes)
	   (type (simple-array (signed-byte 32) *) dat1))
	  (format t "texture size ~a ~%" (list w h))
	  (loop for i below len do
	       (setf (aref bytes i)
		     (floor (* scale 
			       (- (aref dat1 i)
				  min-int)))))
	  (cffi:with-pointer-to-vector-data (ptr bytes)
	    (tex-image-2d target 0 :luminance w h
			  0 :luminance :unsigned-byte ptr)))
	(tex-image-2d target 0 :luminance w h
		      0 :luminance :unsigned-byte (cffi:null-pointer))))
  (andor:start-acquisition)
  (setf start-series (andor:val3 (andor:get-acquisition-progress)))
  (glut:post-redisplay))

(defparameter displayed-images 0)

(defun draw ()
  (clear-color .1 .2 .4 1.)
  (clear :color-buffer-bit)
  (with-pushed-matrix
      (let* ((z 0)
	     (w 1392)
	     (h 1040))
	(with-primitive :quads
	  (tex-coord 0 0 z)
	  (vertex 0 0 z)
	  (tex-coord w 0 z)
	  (vertex w 0 z)
	  (tex-coord w h z)
	  (vertex w h z)
	  (tex-coord 0 h z)
	  (vertex 0 h z))))
  (glut:swap-buffers)


  (loop while (<= (- (andor:val3 (andor:get-acquisition-progress))
		     start-series)
		  displayed-images) do
       (sleep .005))
  (andor:copy-most-recent-data)


  (format t "~a ~%" (list (get-max)))
  (if andor:*im*
      (let* ((w andor:*w*)
	     (h andor:*h*)
	     (len (* w h))
	     (dat andor:*im*)
	     (dat1 (sb-ext:array-storage-vector dat))
	     (bytes (make-array len :element-type '(unsigned-byte 8)))
	     (max-int (get-max))
	     (min-int (get-min))
	     (scale (/ 255f0 (let ((v (- max-int min-int)))
			       (if (= v 0)
				   1
				   v)))))
	(declare 
	 (single-float scale)
	 (type (simple-array (unsigned-byte 8) *) bytes)
	 (type (simple-array (signed-byte 32) *) dat1))
	(format t "got some data ~a~%" (list max-int min-int))
	(loop for i below len do
	     (setf (aref bytes i)
		   (floor (* scale 
			     (- (aref dat1 i)
				min-int)))))
	(cffi:with-pointer-to-vector-data (ptr bytes)
	  (tex-sub-image-2d :texture-rectangle-nv 0 0 0 w h
			    :luminance :unsigned-byte ptr)))
      (format t "got no data~%"))
  (incf displayed-images)
  (glut:post-redisplay))



(defmethod glut:display ((w mk-window))
  (draw))

(defmethod glut:keyboard ((w mk-window) key x y)
  (case key
    (#\Esc (progn (andor:abort-acquisition)
		  (glut:destroy-current-window)))))

(defun run ()
  (glut:display-window (make-instance 'mk-window)))


(andor:init-run-till-abort)
(run)
 
#+nil
(time
 (progn 
   (andor:init :vbin 1 :hbin 1) 
   (andor:request-and-wait) 
   (multiple-value-bind (a b)
       (andor::get-status)
     (format t "~a~%" (list andor::drv-acquiring b)))
   (andor:copy-data)
   nil))
;; this takes nearly 8 seconds! how anoying is that?



;; also i think the wait doesn't work
#+nil
(progn (andor:copy-data) nil)

#+nil
(progn  
  (andor:request-and-wait) 
  (sleep 2)
  (andor:copy-data)
  nil)


#+nil
(aref andor:*im* 0 0)

#+nil
(time
 (andor:exit))

