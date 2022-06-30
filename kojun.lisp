(defvar puzzle-w 8)
(defvar puzzle-h 8)
(defvar region-coords (make-hash-table))
(defvar regions (make-array `(,puzzle-w ,puzzle-h) :initial-contents '(
   (a a b b c d e e)
   (a a f b g d d e)
   (f f f h g i j j)
   (k k k h g i i j)
   (l h h h h i i j)
   (l m n n n o p j)
   (m m m m q o o o)
   (r q q q q o s s)
)))
(defvar puzzle (make-array `(,puzzle-w ,puzzle-h) :initial-contents '(
   (0 0 0 0 0 0 0 0)
   (0 1 3 0 0 0 0 0)
   (0 0 0 0 0 3 0 0)
   (0 0 3 0 0 0 0 0)
   (0 5 0 3 0 0 0 0)
   (0 2 0 0 0 0 0 0)
   (0 0 0 0 0 0 3 0)
   (0 0 5 3 0 0 0 0)
)))

;------------------------------------------------------------------------------;

(defmacro unless* (test &body forms) `(if ,test t (progn ,@forms)))
(defun getmatrix (matrix x y) (aref matrix y x))
(defun sethash (k v dict) (setf (gethash k dict) v))
(defun coords+ (x y) `((,(1- x) ,y) (,(1+ x) ,y) (,x ,(1- y)) (,x ,(1+ y))))
(defun range (l u) (loop for x from (1+ l) below u collect x))

(defun setpuzzle (x y value) (setf (aref puzzle y x) value))
(defun getregion (x y) (getmatrix regions x y))
(defun inbounds (x y) (and (>= x 0) (< x puzzle-w) (>= y 0) (< y puzzle-h)))
(defun get-region-coords (x y) (gethash (getregion x y) region-coords))
(defun get-region-size (x y) (length (get-region-coords x y)))

(defun getpuzzle (coord)
   (let ((x (nth 0 coord)) (y (nth 1 coord)))
   (if (inbounds x y) (getmatrix puzzle x y) 0)))

(defun in-same-region (ax ay bx by) (and
   (inbounds ax ay) (inbounds bx by) (eq (getregion ax ay) (getregion bx by))))

(defun try-get-region-neighbor (ax ay bx by fallback)
   (if (in-same-region ax ay bx by) (getmatrix puzzle bx by) fallback))

;------------------------------------------------------------------------------;

(defun get-orthogonals (x y) (map 'list #'getpuzzle (coords+ x y)))
(defun get-region-numbers (x y) (map 'list #'getpuzzle (get-region-coords x y)))

(defun get-vertical-numbers (x y)
   (let (
      (a (try-get-region-neighbor x y x (1+ y) -1))
      (b (try-get-region-neighbor x y x (1- y) (1+ (get-region-size x y))))
   )
   (range a b)))

(defun get-available-numbers (x y)
   (let (
      (a (get-orthogonals x y))
      (b (get-region-numbers x y))
      (c (get-vertical-numbers x y))
   )
   (set-difference c (union a b))))

;------------------------------------------------------------------------------;

(defun mapregions (x y) (cond
   ((>= y puzzle-h) nil)
   ((>= x puzzle-w) (mapregions 0 (1+ y)))
   (t 
      (let ((region (getregion x y)))
      (if (gethash region region-coords)
         (push `(,x ,y) (gethash region region-coords))
         (sethash region `((,x ,y)) region-coords)
      ))
      (mapregions (1+ x) y))))

(defun trynumbers (x y numbers) 
   (when numbers 
      (setpuzzle x y (first numbers))
      (unless* (solve x (1+ y))
         (setpuzzle x y 0)
         (trynumbers x y (rest numbers)))))

(defun solve (x y) (cond
   ((>= x puzzle-w) t)
   ((>= y puzzle-h) (solve (1+ x) 0))
   (t
      (if (= 0 (getmatrix puzzle x y))
         (trynumbers x y (get-available-numbers x y))
         (solve x (1+ y))))))

;------------------------------------------------------------------------------;

(mapregions 0 0)
(write (if (solve 0 0) puzzle '(no solution found)))