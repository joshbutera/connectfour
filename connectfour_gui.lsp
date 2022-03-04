;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: tictactoe-gui.lsp
;;;;    System: Companions
;;;;   Version: 1.0
;;;;    Author: Katrina Baniak, Josh Butera, Isaac Miller
;;;;   Created: August 30, 2006 14:53:26
;;;;   Purpose: TicTacToe modal interface
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2013-01-23 20:45:11 -0600 (Wed, 23 Jan 2013) $
;;;;  $LastChangedBy: hinrichs $
;;;; ---------------------------------------------------------------------------
;;; This code knows something about the domain, but is optional and
;;; only exposed to the player via a small api.
;;; 


(in-package :games)

(defclass ttt-window (cg:frame-window)
   ((player :initform nil :accessor player)
      (role :initform 'data::oplayer :accessor role)  ; human role
      (mark :initform 'data::o :accessor mark)
      (lines :initform nil :initarg :lines :accessor lines)
      (cells :initform (make-array '(6 7) :initial-element nil) :accessor cells)
      (center-offset :initform (cg::make-position 43 43) :reader center-offset)
      (legal :initform nil :accessor legal) ; currently permissible moves 
      (gate :initform (mp:make-gate t) :accessor gate)
      (latest-move :initform nil :accessor latest-move))
   (:default-initargs
      :lines (list 
         (list (cg:make-position 50 100)
            (cg:make-position 750 100))
         (list (cg:make-position 50 200)
            (cg:make-position 750 200))
         (list (cg:make-position 50 300)
            (cg:make-position 750 300))
         (list (cg:make-position 50 400)
            (cg:make-position 750 400))
         (list (cg:make-position 50 500)
            (cg:make-position 750 500))
         (list (cg:make-position 50 600)
            (cg:make-position 750 600))
         (list (cg:make-position 50 700)
            (cg:make-position 750 700))
         (list (cg:make-position 50 100)
            (cg:make-position 50 700))
         (list (cg:make-position 150 100)
            (cg:make-position 150 700))
         (list (cg:make-position 250 100)
            (cg:make-position 250 700))
         (list (cg:make-position 350 100)
            (cg:make-position 350 700))
         (list (cg:make-position 450 100)
            (cg:make-position 450 700))        
         (list (cg:make-position 550 100)
            (cg:make-position 550 700))         
         (list (cg:make-position 650 100)
            (cg:make-position 650 700))         
         (list (cg:make-position 750 100)
            (cg:make-position 750 700)))))


;;; Try to avoid hanging the IDE:
(defmethod cg:user-close :before ((window ttt-window))
  (when (and (gate window) (not (mp:gate-open-p (gate window))))
    (mp:open-gate (gate window))))


(defmethod cg:redisplay-window ((window ttt-window) &optional box)
  (declare (ignore box))
  (setf (cg::line-width window) 2)
  (call-next-method) ;; Clear the window background
  (dolist (line (lines window))
    (cg:draw-line window (first line) (second line)))
  ;;; Draw entries:
  (dotimes (i 7)
    (dotimes (j 6)
      (let* ((x (1+ i))
             (y (1+ j))
             (val (get-cell window x y)))
        (cond ((null val) nil)
              ((eq val 'data::x) (draw-red window (cell->coord window x y)))
              ((eq val 'data::o) (draw-blue window (cell->coord window x y))))))))

;;; x & y are 1-based coordinates (per GDL)
(defmethod get-cell ((window ttt-window) x y)
  (aref (cells window) (1- x) (1- y)))

(defmethod set-cell ((window ttt-window) x y val &optional (no-update? nil))
  (setf (aref (cells window) (1- x) (1- y)) val)
  (unless no-update? (cg:invalidate window)))

(defmethod coord->cell ((window ttt-window) pos)
  (let* ((pos-x (cg::position-x pos))
         (pos-y (cg::position-y pos))
         (cell-x (cond ((<= pos-x 50) 1)
                       ((>= pos-x 750) 7)
                       (t 6)))
         (cell-y (cond ((<= pos-y 100) 1)
                       ((>= pos-y 700) 6)
                       (t 5))))
    (list cell-x cell-y)))


;;; x & y are 1-based coordinates
;;; Return the top-left cg position of the cell
;;; Remember, Y-positions increase with lower screen positions
(defmethod cell->coord ((window ttt-window) x y)
  (let* ((x-pos (+ 50 (* (1- x) 100)))
         (y-pos (+ 100 (* (1- y) 100))))
    (cg::make-position x-pos y-pos)))

;;; need to figure out how to make these different colors
(defmethod draw-red ((window ttt-window) pos)
   (cg:with-foreground-color (window (red-color window))
      (cg::draw-circle window (cg::position+ pos (center-offset window))
         35)))

(defmethod draw-blue ((window ttt-window) pos)
   (cg:with-foreground-color (window (blue-color window))
      (cg::draw-circle window (cg::position+ pos (center-offset window))
         35)))

;;; update the gui, stash the choice, open the gate, and let the
;;; game process retrieve the move.
(defmethod cg:mouse-left-down ((window ttt-window) buttons cursor-pos)
  (declare (ignore buttons))
  (let* ((context (context (player window)))
         (cell (coord->cell window cursor-pos))
         (move (list 'data::connectfourMark (first cell) (second cell))))
    (cond ((or (member move (legal window) :test #'equal)
               ;; If game didn't pass in legal moves, query on demand:
               (legal-move? (role window) context fire:*reasoner* move))
           (set-cell window (first cell) (second cell) (mark window))
           (setf (latest-move window) move)
           (setf (legal window) nil)
           (mp::open-gate (gate window)))
          (t 
           (cg:beep)))))

;;; start over
(defmethod reset-gui ((window ttt-window) (player game-player) reasoner)
  (declare (ignore reasoner))
  (setf (player window) player)
  (let ((cells (cells window)))
    (dotimes (i 7)
      (dotimes (j 6)
        (setf (aref cells i j) nil))))
  (cg::update-window window))


(defmethod update-gui-state ((window ttt-window) fact-list)
  (dolist (fact fact-list)
    (when (eql (first fact) 'data::connectfourCell)
      (let* ((x (second fact))
             (y (third fact))
             (mark (fourth fact)))
        (set-cell window x y mark t))))
  (cg:invalidate window)
  (cg:update-window window)
  )

;;; Wait for the user to choose a move by setting the legal-moves slot and
;;; waiting on a gate.
;;; Player here is the human player.
(defmethod request-turn ((window ttt-window) human-role legal-moves)
  (let ((mark (if (eq human-role 'data::xplayer)
                'data::red
                'data::blue))
        (gate (gate window)))
    (setf (mark window) mark)
    (setf (legal window) legal-moves)
    (cg:set-foreground-window window)
    (mp::close-gate gate)
    (mp::process-wait "waiting for human opponent" 'mp::gate-open-p gate)
    ;; On waking up, extract the choice and translate as necessary, 
    ;; in order to return one of the legal moves.
    (latest-move window)))

(defun cl-user::ttt ()
  (unless cg:*cg-is-initialized* (cg:initialize-cg))
  (cg:in-cg-process (:process-name "connect-four")
    (prog1
        (setf games::*gui*
          (cg::make-window :ttt-gui
            :class 'ttt-window
            :title "Connect Four"
            :scrollbars nil
            :interior (cg:make-box-relative 400 200 300 300)))
      (cg:update-window *gui*)
      (make-player 'd::Connectfour 'd::redplayer))))