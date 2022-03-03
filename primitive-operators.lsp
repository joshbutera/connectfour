;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: primitive-operators.lsp
;;;;    System: Companions
;;;;   Version: 1.0
;;;;    Author: Tom Hinrichs
;;;;   Created: December 1, 2005 15:05:04
;;;;   Purpose: Implementation of primitive actions
;;;;  $LastChangedDate: 2013-01-23 20:45:11 -0600 (Wed, 23 Jan 2013) $
;;;;  $LastChangedBy: hinrichs $
;;;; ---------------------------------------------------------------------------
;;; 

(in-package :games)

(declaim (special *player* *plan* fire::*debug-htn*))


;;; -----------------------------------------------------------------------------
;;; Primitive definitions
;;;

;;; Execute a domain-level action.
;;; When connected to the gamemaster, this should handle communications and 
;;; verification.

(defun data::doGameAction (role action)
  (unless (fire::ground-expression? action)
    (error "~%Attempt to execute variablized action: ~s" action))
  (unless (legal-move? role (context *player*) fire::*reasoner* action)
    (break "~%Attempted illegal move: ~s" action))
  (let ((gdl-action (make-gdl-action *player* role action)))
    (ggp-trace "~%~s" gdl-action)
    :success))


(defun make-action-stmt (pair)
  (let ((role (first pair))
        (action (second pair)))
    (list 'd::gdl-does role action)))



(defun d::doTell (fact)
  ;; Stores a fact in the reasoner's working-memory.
  ;; Syntax: (doTell <fact>)
  (fire:tell-it (strip-global-context fact) 
    :reasoner fire::*reasoner*
    :reason :plan-execution)
  :success)

(defun strip-global-context (fact)
  (if (and (fire::contextualized-statement? fact)
           (fire::global-context? (fire::context-of-statement fact)))
    (fire::content-of-statement fact)
    fact))


(defun d::doUntell (fact)
  ;; Removes a fact from the reasoner's working-memory.
  ;; Syntax: (doUntell <fact>)
  (let ((reasoner fire::*reasoner*)
        (exp (strip-global-context fact)))
    (cond ((atom exp) nil)
          ((fire::ground-expression? exp)
           (untell-fact exp reasoner))
          (t
           (untell-facts exp reasoner)))
    :success))

(defun untell-facts (pattern reasoner)
  (dolist (fact (fire:ask-it pattern
                  :reasoner reasoner
                  :number :all
                  :facts :wm
                  :infer nil
                  :response :pattern))
    (let ((informant (fire:ltre-informant-of fact reasoner)))
      (fire:untell-it fact 
        :reasoner reasoner 
        :reason informant))))

(defun untell-fact (fact reasoner)
  (let ((informant (fire:ltre-informant-of fact reasoner)))
    (fire:untell-it fact 
      :reasoner reasoner 
      :reason informant)))


;;; Save assertion in the current game case.
;;; If assertion is already contextualized, honor that context.
;;; Modified 7/9/07: No longer provides default context.
(defun data::doRecord (statement)
  (when (and (fire::contextualized-statement? statement)
             (fire::global-context? (fire::context-of-statement statement)))
    (setq statement (fire::content-of-statement statement)))
  (fire::kb-store statement)  ;; maybe use ggp-store here to handle distributed update.
  (fire::tell-it statement :reason :case)
  :success)

;;; Remove assertion from the KB:
(defun data::doForget (statement)
  (when (and (fire::contextualized-statement? statement)
             (fire::global-context? (fire::context-of-statement statement)))
    (setq statement (fire::content-of-statement statement)))
  (let ((informant (fire::ltre-informant-of statement fire::*reasoner*)))
    (fire::untell-it statement :reason informant))
  (fire::kb-forget statement)
  :success)

;;; This stores assertions which are elements of a set.  
;;; Modified 7/9/07: Assumes all elements of assertionSet are properly contextualized.
(defun data::doRecordMembers (assertionSet)
  (dolist (statement-or-conjunct (cdr assertionSet)) ; strip off 'TheSet
    (dolist (statement (fire::possible-conjunction->fact-list
                        statement-or-conjunct))
      (when (and (fire::contextualized-statement? statement)
                 (fire::global-context? (fire::context-of-statement statement)))
        (setq statement (fire::content-of-statement statement)))
      (fire::kb-store statement)
      (fire::tell-it statement :reason :case)))
  :success)

(defun data::doForgetMembers (assertionSet)
  (dolist (statement (cdr assertionSet)) ; strip off 'TheSet
    (when (and (fire::contextualized-statement? statement)
               (fire::global-context? (fire::context-of-statement statement)))
      (setq statement (fire::content-of-statement statement)))
   (let ((informant (fire::ltre-informant-of statement fire::*reasoner*)))
     (fire::untell-it statement :reason informant))
    (fire::kb-forget statement))
  :success)



;;; For tracing and reporting:

(defun data::doAnnounce (msg args)
  ;; ensure fresh line, on whatever stream we're spewing to:
  (apply #'ggp-trace (concatenate 'string "~&" msg) args)
  :success)

;;; The actual checking of condition is handled in the routine can-execute-now?
;;; This just has to succeed if it's ever called.
(defun data::doWait (actor condition)
  (declare (ignore actor condition))
  :success)

;;;
;;; Second-order planning
;;;
;;; This now returns both a result status and a sequence of actions,
;;; thereby saving the general execution routine a lookup step.
;;; **** Incompatible change: Remove the whole notion of actor from the primitive. ****
;;; **** This doesn't change the current-plan at all, but simply returns the expansion. ****
(defun data::doPlan (actor root-task)
  "Creates a linear plan for actor and reifies it."
  (declare (ignore actor))
  ;(ggp-trace "~%Planning ~s" (fire::get-subtasks root-task))
  ;; Unfortunately, this has to be done on every call to the planner, not just
  ;; on each turn.  When plans are deferred until execution time, there may be
  ;; several calls to the planner within a single turn.  Make sure context is up-to-date.
  (initialize-planning-context *player* fire::*reasoner*)
  (multiple-value-bind (action-sequence hierarchical-plan)
      (fire::plan root-task 
        :context (planning-context *player*)
        :facts :wm
        :reasoner fire::*reasoner* 
        :depth 20
        :return-reasons? nil)
    (declare (ignore hierarchical-plan))
    ;(ggp-trace "~&Planner returned: ~S" action-sequence)
    #+common-graphics(cg:process-pending-events-if-event-handler)
    (cond ((or (eq action-sequence :fail) (null action-sequence))
           (ggp-trace "~%Failed to find a plan for ~a, sequence = ~s"
                      root-task action-sequence)
           (values :fail nil))
          (t
           (let ((actions (fire::get-subtasks action-sequence)))
             (values :success actions))))))


;;;; ---------------------------------------------------------------------------
;;;; End of Code