(defpackage tic-tac-toe
  (:use :cl)
  (:export #:game))
  
(in-package :tic-tac-toe)

(defun display-board (board)
  (dotimes (x 3)
    (dotimes (y 3)
      (if (= y 2)
    (format t "~a~%"  (aref board x y))
    (format t "~a | " (aref board x y)))))
  (format t "~%"))

(defun update-board (board coords player)
  (setf (aref board (getf coords :x) (getf coords :y)) player))

(defun valid-position-p (board coords)
  (equal '- (aref board (getf coords :x) (getf coords :y))))

(defun cpu-turn (board)
  (let* ((x (random (array-dimension board 0) (make-random-state t)))
   (y (random (array-dimension board 0) (make-random-state t)))
   (coords `(:x ,x :y ,y)))
    (if (valid-position-p board coords)
  coords
  (cpu-turn board))))

(defun player-turn (board)
  (format t "Please enter X: ")
  (let ((x (parse-integer (read-line) :junk-allowed t)))
    (unless (member x '(0 1 2))
      (player-turn board))

    (format t "Please enter Y: ")
    (let ((y (parse-integer (read-line) :junk-allowed t)))
      (unless (member y '(0 1 2))
  (player-turn board))

      (let ((coords `(:x ,x :y ,y)))
  (if (valid-position-p board coords)
      coords
      (player-turn board))))))

(defun list-equal-p (ls)
  (or (null (rest ls))
      (and (equal (first ls) (second ls))
     (list-equal-p (rest ls)))))

(defun multi-equal (&rest values)
  (list-equal-p values))

(defun game-over-p (board)
  ;; check for draw
  (let ((count 0))
    (destructuring-bind (n m) (array-dimensions board)
      (loop for x below n do
     (loop for y below m do
    (when (equal '- (aref board x y))
      (incf count)))))
    (when (= count 0)
      (return-from game-over-p t)))


  ;; check for win on rows
  (dotimes (x 3)
    (cond
      ((and (multi-equal (aref board x 0)
       (aref board x 1)
             (aref board x 2))
      (not (equal '- (aref board x 0))))
       (return-from game-over-p t))))

  ;; check for win on columns
  (dotimes (x 3)
    (cond
      ((and (multi-equal (aref board 0 x)
       (aref board 1 x)
       (aref board 2 x))
      (not (equal '- (aref board 0 x))))
       (return-from game-over-p t))))
  
  ;; check for win on diagonals
  (cond
    ((and (multi-equal (aref board 0 0)
           (aref board 1 1)
           (aref board 2 2))
    (not (equal '- (aref board 0 0))))
     t)
    ((and (multi-equal (aref board 0 2)
           (aref board 1 1)
           (aref board 2 0))
    (not (equal '- (aref board 0 2))))
     t)

    ;; None of the above are true, return nil!
    (t nil)))
     
(defparameter turn-counter 0)
(defun game (&key (board (make-array '(3 3) :initial-element '-)))
  (when (game-over-p board)
    (display-board board)
    (setf turn-counter 0)
    (format t "Game over")
    (return-from game))

  (display-board board)

  (if (evenp turn-counter)
      (let ((coords (player-turn board)))
        (update-board board coords "x"))

      (let ((coords (cpu-turn board)))
        (update-board board coords "o")))

  (incf turn-counter)
  (game :board board))

(game)

