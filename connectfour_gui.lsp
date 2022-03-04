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
