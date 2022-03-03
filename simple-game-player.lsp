;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                       -*-
;;;; ------------------------------------------------------------------------
;;;; File name: simple-game-player.lsp
;;;;    System: Companions
;;;;   Version: 1.0
;;;;    Author: Tom Hinrichs
;;;;   Created: August 28, 2006 16:20:27
;;;;   Purpose: Game state maintenance code
;;;;  $LastChangedDate: 2013-01-24 08:07:25 -0600 (Thu, 24 Jan 2013) $
;;;;  $LastChangedBy: hinrichs $
;;;; ------------------------------------------------------------------------
;;;
;;; Dumbed down greatly from GGP state maintenance, to avoid learning, 
;;; analysis, analogy, transfer, companions-dependence, etc.
;;; Ie, assume the game microtheory is complete and play it.

(in-package :games)

(defvar *gui* nil "May be bound to a domain-specific gui, if available.")
(defvar *decide-fn* nil 
   "May be bound to a function or method that will strategically 
    decide the next turn.  If null, this plays a legal (but random) game.")
    
(defvar *player* nil "The instance of game-player.")


(defclass game-player ()
  ((domain :documentation "The particular game-domain context (E.g. Tictactoe)"
           :type symbol
           :initarg :domain
           :initform nil
           :accessor domain)
   (context :documentation "The microtheory context of the current game instance"
            :type symbol
            :initform nil
            :accessor context)
   (planning-context :documentation "The PlanSpecificationMicrothoery context"
                     :type symbol
                     :initform nil
                     :accessor planning-context)
   (role :documentation "The game role of this player"
         :type symbol
         :initarg :role
         :initform nil
         :accessor role)
   (state :documentation "The state number"
          :type integer
          :initform 0
          :accessor state)
   (current-plan :documentation "plan to execute this turn."
                 :type list
                 :initform nil
                 :accessor current-plan)
   (delayed-plans :documentation "plans deferred for later execution."
                  :type list
                  :initform nil
                  :accessor delayed-plans)
   (noop-pred :documentation "The predicate denoting no action (if any)"
              :type symbol
              :initform nil
              :accessor noop-pred)
   (blank-token :documentation "The designator for an empty location (if any)"
                :initform nil
                :accessor blank-token)
   (turn-pred :documentation "The 'step' predicate that counts game turns."
              :type symbol
              :initform nil
              :accessor turn-pred)
   (control-pred :documentation "The predicate denoting which role can take action in a turn-taking game"
                 :type symbol
                 :initform nil
                 :accessor control-pred)
   (space-pred :documentation "The predicate indicating the contents of locations (if any)"
               :type symbol
               :initform nil
               :accessor space-pred)
   (space-content-arg :documentation "The arg number denoting contents of a spatial location (if any)."
                      :type number
                      :initform 0
                      :accessor space-content-arg))
  (:documentation "The General Game Player."))



;;; Domain is a symbol denoting game-type context, eg Tictactoe or Minichess
(defun make-player (domain role &key aux-facts)
  ;; If specified, aux-facts will be asserted into the game player's reasoner.
  (unless (and fire::*kb* (fire::open-kb? fire::*kb*))
    (ggp-trace "~%Opening kb..."))
  (cl-user::open-344-kb)
  (let ((reasoner (fire::make-reasoner (format nil "~A-reasoner" domain))))
    (setup-games-reasoner reasoner)
    (setf fire::*optimize-htn* t)
    (setf *player* 
      (make-instance 'game-player :domain domain :role role))
    (initialize-game *player* reasoner)
    (setf fire::*pick-debug-policy* nil)
    (when *gui* (reset-gui *gui* *player* reasoner))
    (dolist (fact aux-facts)
      (fire:tell-it fact
        :reasoner reasoner
        :reason :ggp-setup))
    (main-loop *player* reasoner)
    ))

(defun setup-games-reasoner (reasoner)
  (fire::in-reasoner reasoner)
  (unless (fire::games-source-of reasoner)
    (fire::add-games-source reasoner))
  reasoner)


;;; Construct a new context for the given domain, then extract all the gdl-init
;;; statements and dump them into the context.
(defun initialize-game (player reasoner)
  (let* ((context (setup-game-context player))
         (state-just (fire::contextualize-statement context (state-stmt 0))))
    (setf (state player) 0)
    (setf (context player) context)
;    (when (and (eq *decide-fn* 'plan-game-turn-fn)
;               (unanalyzed-domain? player reasoner))
;      (ggp-trace "~%Analyzing domain before initial game")
;      (analyze-domain player reasoner))
;    (ggp-trace "~%Initializing game context ~s" context)
    (cache-genlMts player reasoner)
    (cache-domain-plans (domain player) reasoner)
    (cache-game-preds player)
    (fire::tell state-just reasoner :ggp context)
    (dolist (init (fire::ask-it 'data::(gdl-init ?state) 
                    :reasoner reasoner :context (domain player) 
                    :facts :wm :response 'data::?state))
      (let ((contextualized-init (list 'd::ist-Information context (list 'd::gdl-true init))))
        (ltre:assert! `(:implies ,state-just ,contextualized-init) :ggp (fire::ltre reasoner))))
    ;; Try to head off some annoying problems:
    (let ((roles (fire::wm-retrieve-it (fire::contextualize-statement (domain player)
                                         'd::(isa ?role GDLRole))
                                       :response 'd::?role)))
      (unless (member (role player) roles)
        (warn "~%Incorrect role (~s) for game (~s), assigning role ~s" (role player) (domain player) (first roles))
        (setf (role player) (first roles))))
    context))  ; return the context

(defun cache-genlMts (player reasoner)
  (fire::tell-it (list 'data::genlMt (planning-context player) (context player)) :reasoner reasoner :reason :cached)
  (fire::tell-it (list 'data::genlMt (context player) (domain player)) :reasoner reasoner :reason :cached)
  (fire::tell-it (list 'data::genlMt (domain player) 'data::GamePlaying) :reasoner reasoner :reason :cached)
  (fire::tell-it (list 'data::genlMt 'data::GamePlaying 'data::HTNPlanner) :reasoner reasoner :reason :cached)
  )


(defun cache-domain-plans (domain reasoner)
  ;; This should catch the definitions of primitives in the particular game and the action model hypotheses:
  (fire::ask-it `(d::ist-Information ,domain d::?fact) :reasoner reasoner :facts :kb :env nil)
  ;; The remainder of plans are currently in the GamePlaying microtheory.
  (fire::ask-it 'd::(ist-Information GamePlaying ?fact) :reasoner reasoner :facts :kb :env nil))


;;; Take a game domain context (eg, "tic-tac-toe") and produce a unique context
;;; representing the next game instance.
(defun setup-game-context (player)
  "Create a new context to represent the game instance"
  (unless (context player)
    (let* ((domain (domain player))
           (planning-context (intern (concatenate 'string (symbol-name domain) "-plan") :data))
           (root-context-string (concatenate 'string (symbol-name domain) "-game"))
           (context (fire::new-spec-mt domain root-context-string)))
    (setf (context player) context)
    (setf (planning-context player) planning-context)
    ;; The planning context is a scratchpad microtheory:
    (ggp-store-globally player `(data::isa ,planning-context data::Microtheory))
    (ggp-store-globally player `(data::genlMt ,planning-context ,domain))
    (setf fire::*current-context* planning-context)
    ))
  (context player))


;;; Pass in the player so we can specialize for companion agents:
(defmethod ggp-store ((player game-player) prop)
  (let ((assertion (fire::contextualize-statement (context player) prop)))
    (fire:tell-it prop
      :reason :ggp
      :context (context player))
    (fire::kb-store assertion)
    assertion))

(defmethod ggp-store-globally ((player game-player) assertion)
  (fire::kb-store assertion)
  assertion)



;;; Cache a few of the relevant predicates where possible.
;;; Do this after caching domain plans, so domain-level facts should be in wm.
(defun cache-game-preds (player)
  (ggp-trace "~%Caching game preds")
  (multiple-value-bind (noop blank turn control space-pred content-arg)
      (game-parameters (domain player) :facts :wm)
    (setf (noop-pred player) noop)
    (setf (blank-token player) blank)
    (setf (turn-pred player) turn)
    (setf (control-pred player) control)
    (setf (space-pred player) space-pred)
    (setf (space-content-arg player) content-arg)))

;;; Split this out so it can be used elsewhere:
(defun game-parameters (context &key (facts :wm))
  (let* ((retriever (if (eq facts :wm) #'fire::wm-retrieve-it #'fire::retrieve-it))
         (noop (first
                (funcall retriever 
                 (fire::contextualize-statement
                  context (list 'd::identityOperator context 'd::?op))
                 :response 'd::?op)))
         (blank (first
                 (funcall retriever 
                  (fire::contextualize-statement
                   context (list 'd::emptyCellToken context 'd::?tok))
                  :response 'd::?tok)))
         (turn (first
                (funcall retriever 
                 (fire::contextualize-statement
                  context (list 'd::turnPredicate context 'd::?step))
                 :response 'd::?step)))
         (control (first
                   (funcall retriever 
                    (fire::contextualize-statement
                     context (list 'd::controlPredicate context 'd::?pred))
                     :response 'd::?pred)))
         (spatial-location-pair 
          (first (funcall retriever 
                     (fire::contextualize-statement
                      context (list 'd::spatialLocationPred context 'd::?pred 'd::?arg))
                   :response 'd::(?pred ?arg))))
         (space-pred (first spatial-location-pair))
         (content-arg (second spatial-location-pair)))
    (values noop blank turn control space-pred content-arg)))


;;; -------------------------------------------------------------------------
;;; Playing the Game
;;;

(defun main-loop (player reasoner)
  (let* ((opponents (opponents player reasoner))
         (opponent (and (= (length opponents) 1) (first opponents))))
    (do ()
        ((game-over? reasoner (context player)))
      #+common-graphics(cg:process-pending-events-if-event-handler)
      (take-one-turn player reasoner)
      (when opponent
        (wait-for-opponent player opponent reasoner)))
    (sleep 0.5)
    (report-winner player reasoner)))


;;; Advance turn by one.
;;; 1) select and execute a move.
;;; 1a) wait for response from gamemaster to verify actual move executed.
;;; 2) assert the 'gdl-does' facts representing moves
;;; 2a) assert holdsIn stmts for case construction.
;;; 3) construct the old and new turn justifications
;;; 4) update the facts in the game context.
(defun take-one-turn (player reasoner)
  (ggp-trace "~%*** State ~s ***" (state player))
  (let* ((context (context player))
         (role (role player))
         (best-move (choose-best-move player reasoner))
         (state-just (fire::contextualize-statement context (state-stmt (state player)))))
    (record-move player role best-move reasoner state-just context)
    (let ((new-state-just 
           (fire::contextualize-statement 
            context (state-stmt (incf (state player))))))
      (update-game state-just new-state-just reasoner player))))


;;; This is ok for now, until we play non-turn-taking games.
(defun opponents (player reasoner)
  (remove (role player)
          (fire::ask-it 'd::(isa ?role GDLRole)
            :reasoner reasoner :context (domain player) :response 'd::?role)))

;;; Return (noop) if it's not role's turn:
(defun maybe-do-noop (player reasoner role)
  (when (control-pred player)      ; If this is a turn-taking game...
    (unless (fire::wm-retrieve-it  ; ... and it's not role's turn...
             (list 'd::ist-Information (context player)
                   (list 'd::gdl-true (list (control-pred player) role)))
             :reasoner reasoner :number 1)
      (list (noop-pred player))))) ; ... then do nothing.
    

;;; Issue: when to increment turn, how to interleave player's actions with opponent's?
;;; This comes about because turn-taking is not part of the game definition language.
;;; TicTacToe encodes turn-taking via a "noop" action which is an action that doesn't
;;; change the state.
;;; We're currently handling this as a round-robin, where this player is in charge.
;;; Probably won't work with two symmetric siblings.

;;; Construct a symbol to denote the current turn (for use as a tms justification)
(defun state-informant (state)
  (intern (format nil "state~a" state) :data))

(defun state-stmt (state)
  (list 'd::StateFn state))

;;; Setup justifications in wm and save action to explicit case:
(defun record-move (player role move reasoner state-just context)
  (declare (ignore player))
  (let* ((prop (list 'd::gdl-does role move))
         (statement (fire::contextualize-statement context prop))
         (state (fire::decontextualize-statement state-just))
         (state-statement (fire::contextualize-statement context 
                            (list 'd::holdsIn state prop))))
    ;; prior turn justifies current move
    (ltre:assert! `(:implies ,state-just ,statement) :ggp (fire::ltre reasoner))
    ;; Don't use ggp-store here because we want to cache in wm as well:
    (cl-user::doRecord state-statement)))


;;; Translate internal vocabulary back to tournament vocabulary:
(defun make-gdl-action (player role move)
  (let* ((move-pred 
          (first (fire::ask-it (list 'd::gdlEquivalent (first move) 'd::?move-pred)
                   :context (domain player)
                   :facts :wm                      ; Added 9-11-07
                   :response 'd::?move-pred)))
         (translated-move
          (when move-pred
            (if (rest move)
              (cons move-pred (rest move))
              move-pred))))
    (when translated-move
      (list 'd::does role translated-move))))

;;; The inverse of the above:
(defun translate-gdl-action (player move)
  (let* ((move-pred 
          (first (fire::ask-it (list 'd::gdlEquivalent 'd::?move-pred (first move))
                   :context (domain player)
                   :facts :wm
                   :response 'd::?move-pred))))
      (when move-pred
        (if (rest move)
          (cons move-pred (rest move))
          move-pred))))

;;; Wait for another player (usually human) to make his move:
;(defun wait-for-opponent (player opponent reasoner)
;  (let ((legal-moves (legal-moves opponent (context player) reasoner)))
;    (cond ((null legal-moves) (error "No legal moves - shouldn't happen."))
;          ((and (eql (length legal-moves) 1)
;                (eql (length (first legal-moves)) 1)  ; Noop is always unary
;                (eql (first (first legal-moves)) (noop-pred player)))
;           (first legal-moves)) ; just do the noop automatically
;          (t
;           (ggp-trace "~%~a legal moves" (length legal-moves))
;           (get-choice-from-opponent opponent legal-moves)))))


;;; This version doesn't pre-compute legal-moves:
(defun wait-for-opponent (player opponent reasoner)
  (let* ((move (or (maybe-do-noop player reasoner opponent)
                  (get-choice-from-opponent opponent nil)))
         (context (context player))
         (state-just (fire::contextualize-statement context (state-stmt (state player)))))
    (record-move player opponent move reasoner state-just context)))


;;; Get user input somehow:
(defun get-choice-from-opponent (role moves)
  (cond ((null *gui*)
         #+:common-graphics (cg:ask-user-for-choice-from-list "legal moves" moves)
         #-:common-graphics nil) ;; need something better here for non-CG systems
        (t 
         (ggp-trace "~%Waiting for ~a..." role)
         (request-turn *gui* role moves))))

;;; Update the current state description.
;;; Old-state and new-state are decontextualized (StateFn <state>) statements
(defun update-game (old-state-just new-state-just reasoner player)
  (let* ((ltre (fire::ltre reasoner))
         (context (context player))
         (fire::*assume-timestamped-cwas-okay* nil)
         (next-stmts (fire::query               ; call query to infer next state of the game.
                         'd::(gdl-next ?stmt)
                       :context context
                       :infer :only ; accept no cached substitutes!
                       :facts :wm
                       :reasoner reasoner
                       :response 'd::?stmt)))
    (when (null next-stmts) (break "No next-stmts!"))
    (fire::untell old-state-just reasoner :ggp context)
    (fire::tell new-state-just reasoner :ggp context)
    (dolist (stmt next-stmts)
      (let ((cstmt (fire::contextualize-statement context (list 'd::gdl-true stmt))))
        (ltre:assert! (list :implies new-state-just cstmt) :ggp ltre)))
    (when *gui*
      (let ((current-state (fire::wm-retrieve-it 
                            (fire::contextualize-statement
                             context 'd::(gdl-true ?fact))
                            :reasoner reasoner
                            :response 'd::?fact)))
        (if (null current-state)
          (break "No current facts for context = ~a" context))
        (update-gui-state *gui* current-state)))
    ;(cl-user::show-game-state)
    new-state-just))

;;; The game is over if we can infer (gdl-terminal).
(defun game-over? (reasoner context)
  (let ((fire::*assume-timestamped-cwas-okay* nil))
    (fire::query 'd::(gdl-terminal)  :context context :reasoner reasoner :facts :wm)))

(defun outcome (reasoner context)
  (let ((fire::*assume-timestamped-cwas-okay* nil))
    (and (game-over? reasoner context)
         (fire::query 'data::(gdl-goal ?player ?score) 
           :context context 
           :reasoner reasoner
           :facts :wm
           :response :pattern))))

(defun report-winner (player reasoner)
  (ggp-trace "~%*** State ~s ***" (state player))
  (let* ((role (role player))
         (stmts (mapcar #'fire::decontextualize-statement
                  (outcome reasoner (context player))))
         (stmt (find 100 stmts :key #'third))
         (winner (and stmt (second stmt)))
         (player-goal (find role stmts :key #'second))
         (score (and player-goal (third player-goal))))
    (record-outcome player stmts)
    (cond (winner (ggp-trace "~%~a won" winner))
          ((> (length stmts) 1)
           (ggp-trace "~%Stalemate at state ~a" (state player))
           (ggp-trace "~% goal statements = ~{~% ~s~}" stmts))
          (score
           (ggp-trace "~%~a's score = ~a" role score))
          (t
           (ggp-trace "~%No score reported at state ~a" (state player))))
      (ggp-trace "~%"))
    )

;;; Save game result to case
(defun record-outcome (player outcomes)
  (let ((statefn (state-stmt (state player))))
    (ggp-store player (list 'd::terminalState statefn))  ; support efficient lookup.
    (dolist (outcome outcomes)
      (ggp-store player (list 'd::holdsIn statefn outcome)))))

(defun cl-user::show-game-state (&optional (player *player*))
  (let* ((context (context player))
         (query (fire::contextualize-statement context 'd::(gdl-true ?stmt)))
         (facts (fire::wm-retrieve-it query :reasoner fire::*reasoner* :response 'd::?stmt)))
    (ggp-trace "~%~a, state ~a" context (state player))
    (dolist (fact facts)
      (ggp-trace "~%~a" fact))
    (values)))



;;;
;;; Decision Making
;;;

;;; The problem with this is that it assumes the planner only generates and executes
;;; domain-level actions.  However, we expect to generate actions that spawn learning
;;; goals, create case libraries, etc.

;;; In the context of a gamemaster-controlled game, the execution of the primitive
;;; must communicate with the gamemaster and verify the actual move taken.  If
;;; we send an illegal move, gamemaster will substitute a legal one.
;;; It appears we're responsible for computing the effects of all actions (including
;;; other player's)
(defun choose-best-move (player reasoner)
  (or (maybe-do-noop player reasoner (role player))  ; short-circuit some reasoning if it's not player's turn
      (if (and *decide-fn* (fboundp *decide-fn*))
        (funcall *decide-fn* player reasoner) ; Invoke FIRE-based planner & executor
        (fire::pick (legal-moves (role player) (context player) reasoner)))))

;;; What are the legal moves for role this turn?
(defun legal-moves (role context reasoner)
  (let ((fire::*assume-timestamped-cwas-okay* nil))
    (fire::query (list 'd::gdl-legal role 'd::?move) 
      :context context :facts :wm :reasoner reasoner :response 'd::?move)))

;;; Given a particular move, is it legal?
(defun legal-move? (role context reasoner move)
  (let ((fire::*assume-timestamped-cwas-okay* nil))
    (fire::query (list 'd::gdl-legal role move)
      :context context :reasoner reasoner :facts :wm :number 1 :response :pattern)))

;;;
;;; GUI Support
;;;

(defgeneric update-gui-state (window fact-list)
  (:documentation "Updates state of any associated gui."))

(defgeneric request-turn (window role legal-moves)
  (:documentation "Requests a choice from the user via any associated gui."))

(defgeneric reset-gui (window player reasoner)
  (:documentation "Clears gui state."))

(defmethod ggp-trace ((ctrl-string string) &rest args)
  (ide:eval-in-listener-thread 
   `(let* ((*package* (find-package :cl-user))
           (pane (cg:find-named-object :listener-pane (cg:screen cg::*system*))))
      (apply #'format pane ,ctrl-string ',args)
      (when (open-stream-p pane) 
        (force-output pane)))
   :listener :gui))

;;;; ------------------------------------------------------------------------
;;;; End of Code
