(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :asdf)
  (require :cl-opengl)
  (require :cl-glut)
  (require :cl-glu))


(defpackage :run
  (:shadowing-import-from :cl close get special)
  (:use :cl :gl :glut))

(in-package :run)


(defclass fenster (window)
  ((cursor-position :accessor cursor-position 
		    :initform (make-array 2 :element-type 'fixnum)
		    :type (simple-array fixnum (2)))
   (draw-func :accessor draw-func
	      :initarg :draw-func
	      :initform #'(lambda ()   
			    (with-primitive :lines
			      (color 1 0 0) (vertex 0 0 0) (vertex 1 0 0)
			      (color 0 1 0) (vertex 0 0 0) (vertex 0 1 0)
			      (color 0 0 1) (vertex 0 0 0) (vertex 0 0 1)))
	      :type function)))

(defgeneric set-view (w))
(defmethod set-view ((w fenster))
      (load-identity)
      (viewport 0 0 (width w) (height w))
      (matrix-mode :projection)
      (load-identity)
      (glu:perspective 40 (/ (width w) (height w)) .1 100)
      (glu:look-at 20 30 -5
		   0 0 0
		   0 0 1)
      (matrix-mode :modelview)
      (load-identity))

(defmethod display ((w fenster))
  (clear :color-buffer-bit :depth-buffer-bit)
  (load-identity)
  
  (funcall (draw-func w))
    
  (swap-buffers)
  (post-redisplay))

(defmethod reshape ((w fenster) x y)
  (setf (width w) x
	(height w) y)
  (set-view w))

(defmethod display-window :before ((w fenster))
  (set-view w))

(defmethod passive-motion ((w fenster) x y)
  (setf (aref (cursor-position w) 0) x
	(aref (cursor-position w) 1) (- (height w) y)))

(defmethod keyboard ((w fenster) key x y)
  (case key
    (#\Esc (destroy-current-window))))

(defmacro with-gui ((w &optional (h w) (x 0) (y 0)) &body body)
  `(display-window 
    (make-instance 'fenster
		   :mode '(:double :rgb :depth)
		   :width ,w :height ,h
		   :pos-x ,x :pos-y ,y 
		   :draw-func #'(lambda ()
				  ,@body))))






(defvar *reinitialize* t)

(progn
  (defparameter *vertex-shader*
    "varying float d;
void main(){
  gl_ClipVertex=gl_ModelViewMatrix*gl_Vertex;
  gl_Position=ftransform();
  d=(43-gl_Position.z)*.022;
}
")
  (defparameter *fragment-shader*
    "varying float d;
void main()
{
 gl_FragColor = vec4(vec3(d),1);
}
")
 (setf *reinitialize* t))





(let* ( )
  (defun compile-and-check (shader)
    (compile-shader shader)
    (when (eq :false (get-shader shader :compile-status))
      (error (get-shader-info-log shader))))
  (defun link-and-check (program)
    (link-program program)
    (when (eq :false (get-program program :link-status))
      (error (get-program-info-log program))))
  (defun my-create-program (vertex-shader fragment-shader)
    (let ((program (gl:create-program)))
      (unless (= 0 vertex-shader)
	(attach-shader program vertex-shader))
      (unless (= 0 fragment-shader)
	(attach-shader program fragment-shader))
      (link-and-check program)
      program))
  (defun compile-shader-source (type stringlist)
    (let ((shader (create-shader type)))
      (shader-source shader stringlist)
      (compile-and-check shader)
      shader))
  (defun init-shader ()
    (let* ((v (compile-shader-source :vertex-shader *vertex-shader*))
	   (f (compile-shader-source :fragment-shader *fragment-shader*))
	   (p (my-create-program v f)))
      (use-program p)
      (check-error "init-shader")))
  (defun init-rendering ()
    (clear-color 0 0 0 1))
  (defun my-init ()
    (matrix-mode :projection)
    (load-identity)
    (glu:perspective 40 1s0 30 40)
      (glu:look-at 20 30 20
		   0 0 0
		   0 0 1)
      (matrix-mode :modelview)

    (enable :depth-test)
    (init-shader)
    (init-rendering))
  (let ((angle 0))
    (declare (type fixnum angle))
   (defun draw ()
     (when *reinitialize*
       (my-init)
       (setf *reinitialize* nil))
    
     (clear-color 0 0 0 1)
     (clear :color-buffer-bit :depth-buffer-bit)
     (with-pushed-matrix 
       (rotate angle 0 0 1)
       (rotate 90 1 0 0)
       (setf angle (if (< angle 360)
	    (1+ angle)
	    0))
       (glut:solid-teapot 8s0))
     (with-primitive :triangles
       (color 1 1 1 1)
       (vertex 10 10)
       (color 1 1 0)
       (vertex 700 10)
       (color 1 0 1)
       (vertex 10 200))
     (check-error "draw")))
  (setf *reinitialize* t))

#+Nil
(sb-thread:make-thread #'(lambda () (with-gui (200 200)
				 (draw)))
		       :name "ogl")
