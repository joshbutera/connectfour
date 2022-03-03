;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                        -*-
;;;; -------------------------------------------------------------------------
;;;; File name: plan-execution.lsp
;;;;    System: Companions/GGP
;;;;   Version: 1.0
;;;;    Author: Tom Hinrichs
;;;;   Created: May 6, 2005 10:01:07
;;;;   Purpose: plan execution, invocation and bookkeeping
;;;;  $LastChangedDate: 2013-01-23 20:45:11 -0600 (Wed, 23 Jan 2013) $
;;;;  $LastChangedBy: hinrichs $
;;;; -------------------------------------------------------------------------

(in-package :games)

(declaim (special *player* *plan* *decide-fn*))

(defvar cl-user::*profile-turn* nil "When this is a number, profile that turn.")
(defvar cl-user::*profile-post-mortem* nil)


;;; Make the game-state maintenance run the planner:
(setf *decide-fn* 'plan-game-turn-fn)

(defun plan-game-turn-fn (player reasoner &optional (depth 0))
  (fire::with-reasoner reasoner
    (when (>= depth 2)
      (let ((choice (fire::pick (legal-moves (role player) (context player) reasoner))))
        (if choice
          (ggp-trace "~&Catastrophic plan failure: picking randomly ~s" choice)
          (break "~&Can't even pick randomly!"))
        (return-from plan-game-turn-fn choice)))
    (unless (current-plan player)
      (let ((role (role player)))
        (set-current-plan
         role 
         (list 'd::actionSequence
               (list 'd::TheList
                     (list 'd::doPlan role
                           (list 'd::actionSequence
                                 (list 'd::TheList
                                       (list 'd::planTurn role)))))))))
    (let ((action (decide-game-turn player)))    ; The action to take this turn
      (unless action
        (warn "No action selected")
        ; Give it one more chance before defaulting to random:
        (setq action (plan-game-turn-fn player reasoner (1+ depth))))
      action)))



;;; Need to retract everything in the planning context and
;;; copy all the gdl-true statements from the execution context.
;;; This is now called from within doPlan, because it needs to be re-run
;;; every time we invoke the planner, not just once per turn.
(defun initialize-planning-context (player reasoner)
  (let* ((pcontext (planning-context player))
         (econtext (context player))
         (facts (fire::wm-retrieve-it 
                 (list 'd::ist-Information econtext 'd::(gdl-true ?fact))
                 :reasoner reasoner :response 'd::?fact)))
    (fire::with-reasoner reasoner
      ;(fire::retract-prior-states pcontext)
      (ltre::kill-microtheory pcontext) ; eliminate prior facts left lying around.
      ;(ggp-trace "  ~%Adding ~a facts to planner context" (length facts))
      (dolist (fact facts)
        (fire::tell (fire::contextualize-statement pcontext (list 'd::gdl-true fact))
                    reasoner :given :all)))))


;;; A single game turn may involve iterated planning/execution actions
;;; possibly involving meta-level learning goals, creating contingency
;;; plans to be picked up later, etc.  These plans should all be examined
;;; before making a decision for the current turn.
(defun decide-game-turn (player)
  (let* ((actor (role player))
         (plan (current-plan player))
         (current-action nil))
    (multiple-value-bind (rtncode rest-plan last-domain-action)
        (execute-ggp-plan player actor plan)
      (when last-domain-action (setq current-action last-domain-action))
      (case rtncode
        (:wait (defer-current-plan actor rest-plan))
        (:fail (replan actor rest-plan))
        (otherwise (clear-current-plan actor))))
    current-action))


;;; execute-ggp-plan executes a linear operational plan by looping through the
;;; plan and invoking functions with the same name as the operator actions.
;;; For ggp domain actions, we trap the primitive and wrap it in
;;; (doGameAction <role> <action>) to support communications &etc.
;;; We only want this to execute one domain action (even if its "turn"
;;; permits multiple actions) so it will have to defer as soon as it hits
;;; a second domain action.
;;; Unlike Freeciv, we don't have to keep track of prior actions since this
;;; isn't event-driven.
(defun execute-ggp-plan (player actor plan &optional latest-action)
  (do* ((operators (fire::get-subtasks plan))
        (operator (car operators) (car operators))
        (latest-action latest-action)  ; The game-level action. Update in body of loop
        (result :success)
        (new-operators nil))
       ((endp operators) (values result nil latest-action))
    (cond ((not (can-execute-now? operator latest-action))
           (if (eq (first operator) 'data::doDefer)
             (setq operator (pop operators)))
           (return-from execute-ggp-plan (values :wait operators latest-action)))
          ((eq (first operator) 'data::doDefer) 
           (pop operators))                         ; skip over extra defers.
          ((and (not (domain-action? operator))
                (not (fboundp (car operator))))
           (warn "~%   Operator not implemented: ~s" operator)
           (return-from execute-ggp-plan (values :fail operators latest-action)))
          (t
           (when (domain-action? operator)
             (setq latest-action operator)
             (setq operator (list 'd::doGameAction actor operator)))
           (multiple-value-setq (result new-operators)
             (do-primitive-action player (car operator) (cdr operator)))
           (when (member result '(:fail :wait))
             (return-from execute-ggp-plan (values result operators latest-action)))
           (pop operators)
           (when (and (eql (car operator) 'd::doPlan)
                      (eql (cadr operator) actor)
                      new-operators)
             (setq operators (append new-operators operators)))))))

;;; Break this out so we can specialize it for agents in a companion:
(defmethod do-primitive-action ((player game-player) (primitive t) (args t))
  (apply primitive args))


;;; Attempt to localize turn-specific information here.
;;; If we haven't done a game action yet this turn, then doDefer should have
;;; no effect, and any operator should be executable.
;;; If we have done an action, then allow inferential tasks to tailgate
;;; unless preceded by doDefer:
(defun can-execute-now? (operator did-game-action)
  (or (not did-game-action)
      (and (fire::inferential-task? operator (planning-context *player*) :wm)
           (not (eq (first operator) 'data::doDefer)))))

;;; Return t iff this is a game-specific action
(defun domain-action? (operator)
  (and (fire::instance-of? (car operator) 'd::SimpleActionPredicate (domain *player*))
       (fire::instance-of? (car operator) 'd::GamePredicate (domain *player*))))


;;; Allow a plan to wait for a condition to become true before proceeding:
(defun condition-satisfied? (query)
  (fire::query query
    :reasoner fire::*reasoner*
    :context (context *player*)))


(defun get-current-plan (actor)
  (when (eq actor (role *player*))
    (current-plan *player*)))


;;; After executing a plan, get rid of it.
(defun clear-current-plan (actor)
  (set-current-plan actor nil))  

;;; Store an actor's current plan in the player, deleting old plans if necessary.
(defun set-current-plan (actor plan)
  (when (eq actor (role *player*))
    (setf (current-plan *player*) plan))
    (when plan
      (fire::get-subtasks plan)))


;;; Revise an actor's current plan
(defun update-current-plan (actor new-operators)
  (let ((new-plan (list 'd::actionSequence (cons 'd::TheList new-operators))))
    (set-current-plan actor new-plan)))

;;; Allow partial execution by truncating the plan to the unexecuted part
;;; and saving for next turn.
(defun defer-current-plan (actor new-operators)
  (if new-operators
    (set-current-plan actor (list 'd::actionSequence (cons 'd::TheList new-operators)))
    (clear-current-plan actor)))


;;; Tack new operators on to front of plan
(defun prepend-current-plan (actor plan new-operators)
  (let* ((old-operators (and plan (fire::get-subtasks plan))))
    (unless (plan-prefix? new-operators old-operators)
      (let ((new-plan (list 'd::actionSequence
                            (cons 'd::TheList
                                  (append new-operators old-operators)))))
        (set-current-plan actor new-plan)))))

(defun plan-prefix? (seq1 seq2)
  (let ((mismatch (mismatch seq1 seq2 :test #'equal)))
    (or (null mismatch)
        (eql mismatch (length seq1)))))
  
;;; This kind of failure should fall back on analogy and/or spawn a learning goal.
;;; It means there's a hole in our planning strategies.
;;; It may also mean popping up a level to a higher goal.
(defun replan (actor remaining)
  (ggp-trace "~%Need to replan for ~a: ~s" actor remaining)
  (clear-current-plan actor)  ; **** Just start over for now
  )


;;;; ---------------------------------------------------------------------------
;;;; End of Code