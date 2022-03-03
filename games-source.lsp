;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                        -*-
;;;; -------------------------------------------------------------------------
;;;; File name: games-source.lsp
;;;;    System: FreeCiv
;;;;    Author: Tom Hinrichs
;;;;   Created: August 23, 2006 16:34:19
;;;;  Modified: Monday, February 8, 2010 at 20:29:35 by Tom
;;;; -------------------------------------------------------------------------
;;;
;;; Massively stripped down to omit all reflection and analogy
;;;


;;; Outsourced predicates:
;;; -------------------------
;;; (gameDomainContext ?mt)
;;; (executionContext ?mt)
;;; (planningContext ?mt)
;;; (gdlRule ?mt ?rule)
;;; (currentRole ?role)  ; May want to change this so it's easier for planner to do minimax


(in-package :fire)

(defclass games-source (source)
  ())

(defmethod games-source? ((thing t)) nil)
(defmethod games-source? ((thing games-source)) t)

(defmethod games-source-of ((thing t)) nil)
(defmethod games-source-of ((thing reasoner))
  (dolist (source (sources thing))
    (when (games-source? source)
      (return-from games-source-of (values source)))))


(defmethod add-games-source ((reasoner reasoner))
  (let ((source (make-instance 'games-source :reasoner reasoner)))
    (add-source source reasoner)
    (register-simple-handler data::gameDomainContext
                             game-domain-test
                             (:known))
    (register-simple-handler data::gameDomainContext
                             get-game-domain
                             (:variable))
    (register-simple-handler data::executionContext
                             execution-context-test
                             (:known))
    (register-simple-handler data::executionContext
                             get-execution-context
                             (:variable))
    (register-simple-handler data::planningContext
                             planning-context-test
                             (:known))
    (register-simple-handler data::planningContext
                             get-planning-context
                             (:variable))
    (register-simple-handler data::currentRole
                             current-role-test
                             (:known))
    (register-simple-handler data::currentRole
                             get-current-role
                             (:variable))
    source))


;;; gameDomainContext

(defsource-handler game-domain-test (mt)
  (when (eq mt (games::domain games::*player*))
    (justify-games-result (list 'd::gameDomainContext mt) (reasoner source) :games context)
    (generate-single-ask-response nil context)))

(defsource-handler get-game-domain ()
  (let ((domain (games::domain games::*player*)))
    (when domain
      (justify-games-result (list 'd::gameDomainContext domain) (reasoner source) :games context)
      (generate-single-ask-response
       (list (cons (second query) domain)) context))))

;;; executionContext

(defsource-handler execution-context-test (mt)
  (when (eq mt (games::context games::*player*))
    (justify-games-result (list 'd::executionContext mt) (reasoner source) :games context)
    (generate-single-ask-response nil context)))

(defsource-handler get-execution-context ()
  (let ((execution-context (games::context games::*player*)))
    (when execution-context
      (justify-games-result (list 'd::executionContext execution-context) (reasoner source) :games context)
      (generate-single-ask-response
       (list (cons (second query) execution-context)) context))))

;;; planningContext

(defsource-handler planning-context-test (mt)
  (when (eq mt (games::planning-context games::*player*))
    (justify-games-result (list 'd::planningContext mt) (reasoner source) :games context)
    (generate-single-ask-response nil context)))

(defsource-handler get-planning-context ()
  (let ((planning-context (games::planning-context games::*player*)))
    (when planning-context
      (justify-games-result (list 'd::planningContext planning-context) (reasoner source) :games context)
      (generate-single-ask-response
       (list (cons (second query) planning-context)) context))))


;;; currentRole

(defsource-handler current-role-test (role)
  (when (eq role (games::role games::*player*))
    (justify-games-result (list 'd::currentRole role) (reasoner source) :games context)
    (generate-single-ask-response nil context)))

(defsource-handler get-current-role ()
  (when games::*player*
    (let ((role (games::role games::*player*)))
      (justify-games-result (list 'd::currentRole role) (reasoner source) :games context)
      (generate-single-ask-response 
       (list (cons (second query) role)) context))))



;;; If we're specifying an antecedent, then the fact is conditional on some other fact.
;;; In that case, if the context is not the game context, then the fact may not be true
;;; in the game context, so don't justify it there.  (eg, see nextCoastlinePos)
;;; Otherwise, the fact is just a datastructure lookup which is certain to be true in the
;;; game context as well as the query context (if they're not the same)
(defmethod justify-games-result ((decontextualized-fact list) (reasoner reasoner) 
                              reason context &optional ante)
  (when (global-context? context)
    (warn "Justifying game fact in global context: ~s" decontextualized-fact))
  (cond ((null ante)
         (tell decontextualized-fact reasoner reason (games::context games::*player*))
         (unless (eq context (games::context games::*player*))
           (tell decontextualized-fact reasoner reason context)))  ; query context
        (t
         (let ((consequent (contextualize-statement context decontextualized-fact)))
           (ltre:assert! (list :implies ante consequent) reason (ltre reasoner))))))


;;;; ---------------------------------------------------------------------------
;;; END OF CODE
