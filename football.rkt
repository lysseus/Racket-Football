#lang racket

(require 2htdp/universe
         2htdp/image
         (only-in racket/gui
                  play-sound))

;;-----------------------------------------------------------------------------------------
;; The Playing Field:
;; The field is conceptually 100 yards long, arranged
;; as a grid of rows and columns. Regardless of which team
;; has the ball play always starts at the 20 yard line and
;; progresses to the 100 yard line.
;;
;; The display of the playing field is a matrix of rows and columns
;; determined by the FIELD-DISP- variables and is a small subset of
;; the overall field. The view shifts depending on who has the ball,
;; the home team is on the left, the visitors on the right. 
;;------------------------------------------------------------------------------------------

(define GOAL-LINE 100)
(define FIELD-DISP-ROWS 3)
(define FIELD-DISP-COLS 9)

;; Quarter-related data.
(define QTR-START-TIME 15)
(define 1ST-QTR 1)
(define 2ND-QTR 2)
(define 4TH-QTR 4)

;; Down-related data.
(define 1ST-DOWN-YTG 10)
(define 1ST-DOWN 1)
(define 4TH-DOWN 4)

;; Possession-related data.
(define HOME-POSSESSION #t)
(define INIT-YL 20)

;; Points awarded during play for touchdown and field goal.
(define POINTS-FIELD-GOAL 3)
(define POINTS-TOUCHDOWN 7)

;; Game Status 
(define GSTAT-SCRIMMAGE 'Scrimmage)
(define GSTAT-IN-PLAY   'In-Play)
(define GSTAT-IN-KICK   'Kicking)
(define GSTAT-PLAY-END  'Play-End)
(define GSTAT-GAME-OVER 'Game-Over)

;; Event Status
(define EVENT-TICK       "Time passes...")
(define EVENT-DIFF?      "Toggle difficulty")
(define EVENT-ST/SC      "Update score/status")
(define EVENT-RB-UP      "Move RB up")
(define EVENT-RB-DOWN    "Move RB down")
(define EVENT-RB-FORWARD "Move RB forward")
(define EVENT-TACKLE     "Tackle!")
(define EVENT-FIELD-GOAL "Field Goal!")
(define EVENT-TOUCHDOWN  "Touchdown!")
(define EVENT-PUNT       "Punt")
(define EVENT-QTR-OVER   "Qtr Over")
(define EVENT-RESTART    "Restart Game")

;; Sounds
;; 2 whistles for punt
;; charge! for field-goal
;; signle whistle for tackle
(define SOUND-PATH "sounds/")
(define SOUND-KICK       (string-append SOUND-PATH "kick.mp3"))
(define SOUND-QTR-OVER   (string-append SOUND-PATH "long-whistle.wav"))
(define SOUND-FIELD-GOAL (string-append SOUND-PATH "cheering-w-horn.wav"))
(define SOUND-PUNT       (string-append SOUND-PATH "blows-3-whistle.mp3"))
(define SOUND-TOUCHDOWN  (string-append SOUND-PATH "cheering-w-whistling.wav"))
(define SOUND-TACKLE     (string-append SOUND-PATH "short-whistle2.wav"))
(define SOUND-RESTART    (string-append SOUND-PATH "fanfare-short.wav"))


;; Player Struct
;; row : player's position vertically on the field beginning from 0 (top).
;; col : player's position horizontally on the field. 
(struct player (row col) #:mutable #:transparent)

;; running-bak struct
;; The running-back inherits from player.
(struct running-back player ((tackled? #:auto))
  #:auto-value #f #:mutable #:transparent)

;; defensive-tackle struct
;; The defensive-tackle inherits from player. It provides the tackle field.
(struct defensive-tackle player ((tackling? #:auto) (nviz? #:auto))
  #:auto-value #f #:mutable #:transparent)

;; initialize-players: ws -> ws
;; Sets up the player scrimmage line. The pattern
;; is based on field position, not the field display,
;; so the running-back is always on the left relative
;; to the defensive-tackles.
;;
;; . . . - . . . . .
;; - . . - . - . . -
;; . . . - . . . . . 
(define (initialize-players ws)
  (define yl (world-yl ws))
  (set-world-players! ws (list (running-back 1 yl)
                               (defensive-tackle 0 (+ yl 3))
                               (defensive-tackle 1 (+ yl 3))
                               (defensive-tackle 2 (+ yl 3))
                               (defensive-tackle 1 (+ yl 5))
                               (defensive-tackle 1 (+ yl 8))))
  ws)

;; compute-ytg: ws -> number? 
;; Computes the team's yards-to-go for 1st down based on
;; their field position and will reflect < 10 when they are
;; closer than 10 yards to the goal line.
(define (compute-ytg ws)
  (if (< (- GOAL-LINE (world-yl ws)) 1ST-DOWN-YTG)
      (- GOAL-LINE (world-yl ws))
      1ST-DOWN-YTG))

;; initialize-play: ws -> ws
;; Sets up the play and players, resetting 1st down if needed.
(define (initialize-play ws)
  (initialize-players ws)
  ;; Set the display start yardline where the ball is.
  (set-world-disp-start-yl! ws (world-yl ws))
  ;; Initialize kick-yards
  (set-world-kick-yards! ws INIT-KICK-YARDS)
  (set-world-kick-yl! ws INIT-KICK-YL)
  ;; Check for 1st down.
  (when (zero? (world-ytg ws))
    ;; We have a 1st down.
    (set-world-down! ws 1ST-DOWN)
    ;; Set ytg to 10 or less if goal line < 10.
    (set-world-ytg! ws (compute-ytg ws)))
  (set-world-gstat! ws GSTAT-SCRIMMAGE)
  ws)

;; The world state. 
(struct world (event ; last significant game event?
               gstat ; game status?
               diff? ; playing difficulty?
               home? ; ball possession?
               qtr   ; game quarter?
               time  ; time remaining in quarter?
               down  ; down? 
               yl    ; ball is on what yardline?
               disp-start-yl 
               ytg   ; yards to go for 1st down
               kick-yards ; how far did RB kick the ball?
               kick-yl    
               home        ; home team score
               visitor     ; visitor team score
               players)
  #:mutable #:transparent)

(define INIT-EVENT #f)
(define INIT-GSTAT #f)
(define INIT-DIFF? #f)
(define INIT-KICK-YARDS 0)
(define INIT-KICK-YL 0)
(define INIT-HOME-SCORE 0)
(define INIT-VISITOR-SCORE 0)
(define INIT-PLAYERS empty)

;; new-world: -> ws
;; Creates an initialized world-state.
(define (new-world #:home? (home? HOME-POSSESSION)
                   #:qtr (qtr 1ST-QTR)
                   #:time (time QTR-START-TIME)
                   #:down (down 1ST-DOWN)
                   #:yl (yl INIT-YL)
                   #:ytg (ytg 1ST-DOWN-YTG)
                   #:home (home INIT-HOME-SCORE)
                   #:visitor (visitor INIT-VISITOR-SCORE))
  ;; Play fanfare
  (play-sound SOUND-RESTART #t)
  ;; Create new world-state and initialize play.
  (initialize-play (world INIT-EVENT
                          INIT-GSTAT
                          INIT-DIFF?
                          home?            ; HOME-POSSESSION
                          qtr             ; 1ST-QTR
                          time            ; QTR-START-TIME
                          down            ; 1ST-DOWN
                          yl              ; INIT-YL
                          yl              ; INIT-YL
                          ytg             ; 1ST-DOWN-YTG
                          INIT-KICK-YARDS
                          INIT-KICK-YL
                          home            ; INIT-HOME-SCORE
                          visitor         ; INIT-VISITOR-SCORE
                          INIT-PLAYERS)))

;; gstat?: ws sta -> boolean?
;; Returns true if the game status is one of sta;
;; false otherwise. 
(define (gstat? ws . sta)
  (list? (member (world-gstat ws) sta)))

;; qtr-over?: ws -> boolean?
;; Asks the world whether the quarter is over.
(define (qtr-over? ws) (zero? (world-time ws)))

;; set-qtr-over: ws ->|
;; Tell the world the quarter is over.
(define (set-qtr-over! ws)
  (play-sound SOUND-QTR-OVER #t)
  (set-world-event! ws EVENT-QTR-OVER)
  (set-world-gstat! ws GSTAT-PLAY-END))

;; touchdown?: ws -> boolean?
;; Asks the world whether a touchdown has occurred.
(define (touchdown? ws)
  (= (player-col (first (world-players ws))) GOAL-LINE))

;; set-touchdown!: ws ->|
;; Tell the world that the rb made a touchdown.
(define (set-touchdown! ws)
  (play-sound SOUND-TOUCHDOWN #t)
  (set-world-event! ws EVENT-TOUCHDOWN)
  (set-world-gstat! ws GSTAT-PLAY-END))

;; punt?: ws -> boolean?
;; Asks the world whether a punt has occurred.
(define (punt? ws) (and (positive? (world-kick-yards ws))
                        (< (+ (world-yl ws) (world-kick-yards ws)) GOAL-LINE)))

;; set-punt!: ws ->|
;; Tells the world that a punt has occurred.
(define (set-punt! ws)
  (play-sound SOUND-PUNT #t)
  (set-world-event! ws EVENT-PUNT)
  (set-world-gstat! ws GSTAT-PLAY-END))

;; field-goal?: ws -> boolean?
;; Asks the world whether a field-goal has occurred.
(define (field-goal? ws) (and (positive? (world-kick-yards ws))
                              (= (+ (world-yl ws) (world-kick-yards ws)) GOAL-LINE)))

;; set-field-goal!: ws ->|
;; Tells the world that a field-goal has occurred.
(define (set-field-goal! ws)
  (play-sound SOUND-FIELD-GOAL #t)
  (set-world-event! ws EVENT-FIELD-GOAL)
  (set-world-gstat! ws GSTAT-PLAY-END))

;; set-tackle!: tackler ws ->|
;; Tells the world that the rb has been tackled and by whom.
(define (set-tackle! tackler ws)
  (play-sound SOUND-TACKLE
              #f)
  (define rb (first (world-players ws)))
  (set-running-back-tackled?! rb #t)
  (set-defensive-tackle-tackling?! tackler #t)
  (set-world-event! ws EVENT-TACKLE)
  (set-world-gstat! ws GSTAT-PLAY-END))


;;;========================================================================================
;;; ON-TICK Section
;;; ---------------
;;; The following define the elements required by Big Bang's on-tick handler.
;;;========================================================================================


;; next-qtr: ws -> ws
;; The quarter is over. We end the game if it's 4th
;; quarter, or pass 
(define (next-qtr ws)
  (cond
    [(= (world-qtr ws) 4TH-QTR) (set-world-gstat! ws GSTAT-GAME-OVER)]
    [else
     ;; 2nd quarter ends, visitor gets the ball, 1st down on the 20 yl. 
     (when (= (world-qtr ws) 2ND-QTR)
       ;; Visitors have the ball...
       (set-world-home?! ws (not HOME-POSSESSION))
       ;; ... on the 20 yl...
       (set-world-yl! ws INIT-YL)
       ;; ... with 10 yards to go.
       (set-world-ytg! ws (compute-ytg ws))
       (set-world-down! ws 1ST-DOWN))
     ;; increment the qtr.
     (set-world-qtr! ws (add1 (world-qtr ws)))
     ;; Reset the clock for the new qtr.
     (set-world-time! ws QTR-START-TIME)])
  ws)


;; check-for-tackler: row col players -> or/c player? #f
;; Returns the player who matches the candidate moves row
;; and col. If no player is found the function returns #f.
(define (check-for-tackler row col players)
  (findf (λ (pl) (and (= (player-row pl) row)
                      (= (player-col pl) col)))
         players))

;; rank-defensive-tackles: rb tackles -> listof ranked tackles
;; Rank the defensive-tackles in order of closest to RB. When equally close
;; the defensive-tackle in front of the running-back is preferred to one behind.
(define (rank-defensive-tackles rb dts)
  (sort (map (λ (pl) (list (+ (abs (- (player-row rb) (player-row pl)))
                              (abs (- (player-col rb) (player-col pl))))
                           (- (player-col rb) (player-col pl))
                           pl))
             dts)
        (λ (e1 e2) (cond
                     [(< (car e1) (car e2)) #t]
                     [(and (= (car e1) (car e2)) (< (cadr e1) (cadr e2)))]
                     [else #f]))))

;; try-move: dt rb others -> (list new-row new-col tackler)
;; Returns a list of values representing a move for the defensive-tackle
;; and whether this results in a tackle.  The move selection prefers
;; moving toward the same row as the running-back over moves toward
;; the running-back. Moving away from the running-back is not an option,
;; and avoids issues of moving "off" the field. If no valid move
;; is found for the defensive-tackle then #f is returned.
(define (try-move dt
                  rb
                  others)
  (define dt-row (player-row dt))
  (define dt-col (player-col dt))
  (define rb-row (player-row rb))
  (define rb-col (player-col rb))
  (define tests
    (list
     (λ () (if (< dt-row rb-row) (values (add1 dt-row) dt-col) (values #f #f)))
     (λ () (if (> dt-row rb-row) (values (sub1 dt-row) dt-col) (values #f #f)))
     (λ () (if (< dt-col rb-col) (values dt-row (add1 dt-col)) (values #f #f)))
     (λ () (if (> dt-col rb-col) (values dt-row (sub1 dt-col)) (values #f #f)))))
  (let/cc return
    (for ([test tests])
      (define-values (new-row new-col) (test))
      (unless (false? new-row)
        (define tk (check-for-tackler new-row new-col others))
        (cond
          [(running-back? tk) (return (list new-row new-col #t))]
          [(player? tk) (void)]
          [else (return (list new-row new-col #f))])))
    #f))

;; try-defensive-tackle rb dts -> dt
;; Randomly selects a defensive-tackle from the ranked defensive-tackles
;; based on the size of the list of ranked defensive-tackles and
;; a distribution based on simulated dice rolls.

(define (choose selects
                (dists (make-list (length selects) (/ 1 (length selects))))
                (rnd (random)))
  (define vec (list->vector
               (reverse
                (for/list ([n (range (length dists))])
                  (for/sum ([m (drop-right dists n)]) m)))))
  (list-ref selects
            (vector-member (for/first ([v vec] #:unless (> rnd v)) v) vec)))

;; choose-defensive-tackle: ws dts -> (cand row col tk)
;; Returns a defensive-tackle, new-row, new-col, and indicates whether
;; the move is a tackle.
(define (choose-defensive-tackle ws (dts (rest (world-players ws))))
  (define players (world-players ws))
  (define rb (first players))
  ;; Rank the dts closest to furthest from RB
  (define ranked-dts (map third (rank-defensive-tackles rb dts)))
  ;; Weighted selection (based on throwing 16 20-sided dice
  (define weights '(0.639 0.2944 0.0615 0.005 0.0001))
  ;; Choose a defensive tackle based on weights.
  (define cand (choose ranked-dts weights))
  (define move (try-move cand rb (remove cand players)))
  (cond
    [(list? move) (apply values (cons cand move))]
    [else (choose-defensive-tackle ws (rest dts))]))

;; move-defensive-tackle: ws -> ws
;; When in-play this routine is called with every clock-tick
;; and moves a defensive-tackle toward the running-back.
(define (move-defensive-tackle ws)
  (define players (world-players ws))
  (define dts (rest players))
  (define-values (dt new-row new-col tackle?) (choose-defensive-tackle ws))
  (cond
    [tackle? (set-tackle! dt ws)]
    [else (set-player-row! dt new-row)
          (set-player-col! dt new-col)])
  ws)

;; toggle-nviz?: ws ->|
;; Toggle the tackling player's nviz?
(define (toggle-nviz? ws)
  (define dts (rest (world-players ws)))
  (define dt (findf (λ (pl) (defensive-tackle-tackling? pl)) dts))
  (when dt (set-defensive-tackle-nviz?! dt (not (defensive-tackle-nviz? dt)))))

(define (ball-in-flight ws)
  (cond
    [(< (world-kick-yl ws) (world-kick-yards ws))
     (set-world-kick-yl! ws (add1 (world-kick-yl ws)))]
    [(field-goal? ws) (set-field-goal! ws)]
    [else (set-punt! ws)]))

;; clock-tick-handler: ws -> ws
;; Big Bang's clock handler. 
(define (clock-tick-handler ws)
  ;; Game time passes only when we are in-play.
  (when (gstat? ws GSTAT-IN-PLAY)
    (set-world-event! ws EVENT-TICK)
    (define div (if (world-diff? ws) 14 28))
    ;; defensive-tackles rush the running-back.
    ;; Converting time back to "ticks", we then move the defensive
    ;; player ever second for easy play and twice as fast for more
    ;; challenging play. 
    (when (zero? (modulo (* 280 (world-time ws)) div))
      (move-defensive-tackle ws))
    (cond
      [(qtr-over? ws) (set-qtr-over! ws)]
      [else
       ;; Decrement time by 0.1 minutes.
       (set-world-time! ws (- (world-time ws) 1/280))]))
  ;; Team has kicked the ball...
  (when (gstat? ws GSTAT-IN-KICK)
    (ball-in-flight ws))
  ;; If the running-back is tackled we blink the tackler. 
  (when (gstat? ws GSTAT-PLAY-END)
    (toggle-nviz? ws))
  ws)


;;;========================================================================================
;;; On-Key Section
;;; --------------
;;; The following define the elements required by Big Bang's on-key handler.
;;;========================================================================================


;;;----------------------------------------------------------------------------------------
;;; Play-End functions.
;;; The following functions are invoked only after play has ended.
;;;-----------------------------------------------------------------------------------------

;; rb-tackled: ws -> ws
;; The running-back was tackled. We have to calculate the down,
;; the yards-to-go, whether ball possession passes to the other
;; team, and set up the new scrimmage.
(define (rb-tackled ws)
  ;; Get the running-back.
  (define rb (first (world-players ws)))
  ;; How many yards did we gain?
  (define yds (- (player-col rb) (world-yl ws)))
  (define made-1st-down? (not (< yds (world-ytg ws))))
  ;; Update the yl from the rb.
  (set-world-yl! ws (player-col rb))
  (cond
    ;; It's 4th down and we didn't make 1st down.
    [(and (= (world-down ws) 4TH-DOWN) (not made-1st-down?))
     ;; the other team gets the ball
     (set-world-home?! ws (not (world-home? ws)))
     ;; Flip the yl for the other team
     (set-world-yl! ws (- GOAL-LINE (world-yl ws)))
     ;; Reset the down to 1st.
     (set-world-down! ws 1ST-DOWN)
     ;; Reset ytg to 10 or less if goal line is < 10.
     (set-world-ytg! ws (compute-ytg ws))]
    ;; We made 1st down.
    [made-1st-down?
     (set-world-down! ws 1ST-DOWN)
     (set-world-ytg! ws (compute-ytg ws))]
    ;; We didn't make 1st down.
    [else
     (set-world-down! ws (add1 (world-down ws)))
     (set-world-ytg! ws (- (world-ytg ws) yds))])
  ws)

;; score-touchdown: ws -> ws
;; Score the touchdown and hand ball possession to the other
;; team. We set 1st down on the yardline on the 20 with 10
;; yards-to-go, and set up the scrimmage.
(define (score-touchdown ws)
  ;; The team gets touchdown points
  ((if (world-home? ws)
       set-world-home!
       set-world-visitor!) ws
                           (+ (if (world-home? ws)
                                  (world-home ws)
                                  (world-visitor ws))
                              POINTS-TOUCHDOWN))
  ;; The other team gets the ball
  (set-world-home?! ws (not (world-home? ws)))
  ;; The other team starts on their 20 yl.
  (set-world-yl! ws INIT-YL)
  ;; Reset the down to 1st.
  (set-world-down! ws 1ST-DOWN)
  ;; Reset ytg.
  (set-world-ytg! ws (compute-ytg ws))
  ws)

;; kicker-punt/field-goal: ws -> ws
;; On a punt the opposing team gets the ball on the its
;; associated yardline.
(define (kicker-punt/field-goal ws)
  ;; It's now 1st down for the opposing team.
  (set-world-down! ws 1ST-DOWN)
  ;; Compute appropriate ytg.
  (set-world-ytg! ws (compute-ytg ws))
  ;; Compute the oppsing team's yl
  (define yl (- GOAL-LINE (+ (world-yl ws) (world-kick-yards ws))))
  ;; If we've crossed the goal line, the new yl is 20.
  (set-world-yl! ws (if (zero? yl) INIT-YL yl))
  ;; 3 points for the team scoring the field goal. 
  (when (zero? yl)
    ((if (world-home? ws) set-world-home! set-world-visitor!) ws POINTS-FIELD-GOAL))
  ;; Give the ball to the opposing team.
  (set-world-home?! ws (not (world-home? ws))))

;; toggle-difficulty: ws ->|
;; Toggles the level of difficulty for game play.
(define (toggle-difficulty ws)
  (unless (gstat? ws GSTAT-IN-PLAY)
    (set-world-event! ws EVENT-DIFF?)
    (set-world-diff?! ws (not (world-diff? ws)))))

(define (status-and-score ws)
  (cond
    ;; Do nothing for these statuses
    [(gstat? ws GSTAT-SCRIMMAGE GSTAT-IN-PLAY) ws]
    [else
     ;; Play Ends with 4 events: quarter over, tackle, touchdown, or kick.
     (set-world-event! ws EVENT-ST/SC)
     (cond
       ;; Quarter over
       [(qtr-over? ws)                 (next-qtr ws)]
       ;; Touchdown
       [(touchdown? ws) (score-touchdown ws)]
       ;; Kick
       [(or (punt? ws) (field-goal? ws)) (kicker-punt/field-goal ws)]
       ;; Tackle
       [else (rb-tackled ws)])
     ;; Set up the players.
     (initialize-play ws)]))

;; kick-the-ball: ws -> ws
;; Kicking can only be done on the 4th down. The kick
;; will be between 1 and 65 yards. If the kick crosses
;; the goal line it's a field goal, otherwise it's a punt.
(define (kick-the-ball ws)
  (when (= (world-down ws) 4TH-DOWN)
    (play-sound SOUND-KICK #t)
    (define (adjusted-ky yl ky)
      (define tmp (+ yl ky))
      (cond
        [(< tmp GOAL-LINE) ky]
        [else (+ ky (- GOAL-LINE (+ yl ky)))]))
    (set-world-kick-yards! ws (adjusted-ky (world-yl ws)
                                           (random 1 65)))
    (set-world-gstat! ws GSTAT-IN-KICK))
  ws)

;;;----------------------------------------------------------------------------------------
;;; In-Play functions.
;;; The following functions are invoked only during play.
;;;-----------------------------------------------------------------------------------------


(define (move-rb-forward ws)
  (set-world-event! ws EVENT-RB-FORWARD)
  ;; Get the running-back from the world state.
  (define rb (first (world-players ws)))
  (define new-col (add1 (player-col rb)))
  (define tk (check-for-tackler (player-row rb)
                                new-col
                                (remove rb (world-players ws))))
  (cond
    ;; Check for touchdown.
    [(touchdown? ws) (set-touchdown! ws)]
    [(player? tk) (set-tackle! tk ws)]
    ;; Move the running-back ahead 1 yard.
    [else
     (set-player-col! rb new-col)
     ;; Check if we need to adjust the display field start.
     (when (= (player-col rb) (+ (world-disp-start-yl ws) FIELD-DISP-COLS))
       (set-world-disp-start-yl! ws (player-col rb)))]))

(define (move-rb-up ws)
  (set-world-event! ws EVENT-RB-UP)
  ;; Get the running-back from the world state.
  (define rb (first (world-players ws)))
  ;; Ignore going up when the rb can't do so.
  (unless (zero? (player-row rb))
    ;; Compute the new row for the rb.
    (define new-row (sub1 (player-row rb)))
    (define tk (check-for-tackler new-row
                                  (player-col rb)
                                  (remove rb (world-players ws))))
    (cond
      ;; running-back was tackled.
      [(player? tk) (set-tackle! tk ws)]
      [else
       ;; Move the running-back up 1 yard.
       (set-player-row! rb new-row)]))
  ws)

(define (move-rb-down ws)
  (set-world-event! ws EVENT-RB-DOWN)
  ;; Get the running-back from the world state.
  (define rb (first (world-players ws)))
  ;; Ignore going down when the rb can't do so.
  (unless (= (player-row rb) (sub1 FIELD-DISP-ROWS))
    ;; Compute the new row for the rb.
    (define new-row (add1 (player-row rb)))
    (define tk (check-for-tackler new-row
                                  (player-col rb)
                                  (remove rb (world-players ws))))
    (cond
      ;; running-back was tackled.
      [(player? tk) (set-tackle! tk ws)]
      [else
       ;; Move the running-back down 1 yard.
       (set-player-row! rb new-row)]))
  ws)

(define (move-rb ws dir)
  (when (gstat? ws GSTAT-SCRIMMAGE)
    (set-world-gstat! ws GSTAT-IN-PLAY))
  (when (gstat? ws GSTAT-IN-PLAY)
    (cond
      [(eq? dir 'f) (move-rb-forward ws)]
      [(eq? dir 'u) (move-rb-up ws)]
      [(eq? dir 'd) (move-rb-down ws)]))
  ws)

;; restart: ws -> ws
;; Restart the game, returning a new world-state
;;; and setting event to indicate the restart.
(define (restart ws)
  (define ws (new-world))
  (set-world-event! ws EVENT-RESTART)
  ws)

(define (key-stroke-handler ws key-val)
  (define ke (string-downcase key-val))
  (cond
    [(key=? ke "r")     (restart ws)]
    [(key=? ke "d")     (toggle-difficulty ws) ws]
    [(key=? ke "s")     (status-and-score ws) ws]
    [(key=? ke "k")     (kick-the-ball ws) ws]
    [(key=? ke (if (world-home? ws) "right" "left")) (move-rb ws 'f) ws]
    [(key=? ke "up")    (move-rb ws 'u) ws]
    [(key=? ke "down")  (move-rb ws 'd) ws]
    [else ws]))

;;;========================================================================================
;;; TO-DRAW Section
;;; ---------------
;;; The following define the elements required by Big Bang's to-draw handler.
;;;========================================================================================


(define MT-WIDTH  800)
(define MT-HEIGHT 400)
(define MT-COLOR 'blue)
(define MT (empty-scene MT-WIDTH MT-HEIGHT MT-COLOR))
(define MT-NAME "Racket Football")

(define LBL-SIZE 20)
(define LBL-COLOR 'yellow)
(define DATA-SIZE 20)
(define DATA-COLOR 'white)
(define SPC-WIDTH 5)
(define SPC-HEIGHT SPC-WIDTH)
(define SPC-COLOR 'white)
(define IMG-PAD 12)

(define (pad img w-pad (h-pad w-pad) (bg-color 'transparent))
  (overlay img (rectangle (+ (image-width img) w-pad)
                          (+ (image-height img) h-pad)
                          'solid bg-color)))

(define (draw-status-and-score-board ws)
  (define qtr-lbl (text "Qtr" LBL-SIZE LBL-COLOR))
  (define qtr-dat (text (~a (world-qtr ws)) DATA-SIZE DATA-COLOR))
  (define qtr-img (pad (above qtr-lbl qtr-dat) IMG-PAD))

  (define down-lbl (text "Down" LBL-SIZE LBL-COLOR))
  (define down-dat (text (~a (world-down ws)) DATA-SIZE DATA-COLOR))
  (define down-img (pad (above down-lbl down-dat) IMG-PAD))
  
  (define home-lbl (text "Home" LBL-SIZE LBL-COLOR))
  (define home-dat (text (~a (world-home ws)) DATA-SIZE DATA-COLOR))
  (define home-img (pad (above home-lbl home-dat) IMG-PAD))

  (define hspc1 (rectangle (max (image-width qtr-img)
                                (image-width down-img)
                                (image-width home-img))
                           SPC-HEIGHT
                           'solid SPC-COLOR))

  (define col1 (above hspc1
                      qtr-img
                      hspc1
                      down-img
                      hspc1
                      home-img
                      hspc1))
  
  (define home?-lbl (text "Possession" DATA-SIZE DATA-COLOR))
  (define home?-dat (text (~a (if (world-home? ws) "Home" "Visitor"))
                          DATA-SIZE DATA-COLOR))
  (define home?-img (pad (above home?-lbl home?-dat) IMG-PAD))
  
  (define yl-lbl (text "Field Position" LBL-SIZE LBL-COLOR))
  (define yl-dat (text (format "~a\t~a\t~a"
                               (if (world-home? ws)
                                   (if (< (world-yl ws) 50) "-|" "  ")
                                   (if (< (world-yl ws) 50) "  " "-|"))
                               (if (< (world-yl ws) 50)
                                   (world-yl ws)
                                   (- GOAL-LINE (world-yl ws)))
                               (if (world-home? ws)
                                   (if (> (world-yl ws) 50) "|-" "  ")
                                   (if (> (world-yl ws) 50) "  " "|-"))) DATA-SIZE DATA-COLOR))
  (define yl-img (pad (above yl-lbl yl-dat) IMG-PAD))
  
  (define time-lbl (text "Time Remaining" LBL-SIZE LBL-COLOR))
  (define time-dat (text (real->decimal-string (world-time ws) 1) DATA-SIZE DATA-COLOR))
  (define time-img (pad (above time-lbl time-dat) IMG-PAD))

  (define hspc2 (rectangle (max (image-width home?-img)
                                (image-width yl-img)
                                (image-width time-img))
                           SPC-HEIGHT
                           'solid SPC-COLOR))

  (define col2 (above hspc2
                      home?-img
                      hspc2
                      yl-img
                      hspc2
                      time-img
                      hspc2))
  
  (define yards-lbl (text "Yards" LBL-SIZE LBL-COLOR))
  (define yards-dat
    (text (~a (cond
                ;; kick
                [(gstat? ws GSTAT-IN-KICK) (world-kick-yl ws)]
                [(positive? (world-kick-yards ws))
                 (world-kick-yards ws)]
                [else (- (player-col (first (world-players ws)))
                         (world-yl ws))]))
          DATA-SIZE DATA-COLOR))
  (define yards-img (pad (above yards-lbl yards-dat) IMG-PAD))
  
  (define ytg-lbl (text "Yards To Go" LBL-SIZE LBL-COLOR))
  (define ytg-dat (text (~a (world-ytg ws)) DATA-SIZE DATA-COLOR))
  (define ytg-img (pad (above ytg-lbl ytg-dat) IMG-PAD))

  (define visitor-lbl (text "Visitor" LBL-SIZE LBL-COLOR))
  (define visitor-dat (text (~a (world-visitor ws)) DATA-SIZE DATA-COLOR))
  (define visitor-img (pad (above visitor-lbl visitor-dat) IMG-PAD))

  (define hspc3 (rectangle (max (image-width yards-img)
                                (image-width ytg-img)
                                (image-width visitor-img))
                           SPC-HEIGHT
                           'solid SPC-COLOR))

  (define col3 (above hspc3
                      yards-img
                      hspc3
                      ytg-img
                      hspc3
                      visitor-img
                      hspc3))
  
  (define diff?-lbl (text "Difficulty?" LBL-SIZE LBL-COLOR))
  (define diff?-dat (text (~a (if (false? (world-diff? ws)) "1" "2")) DATA-SIZE DATA-COLOR))
  (define diff?-img (pad (above diff?-lbl diff?-dat) IMG-PAD))
  
  (define gstat-lbl (text "Status" LBL-SIZE LBL-COLOR))
  (define gstat-dat (text (~a (world-gstat ws)) DATA-SIZE DATA-COLOR))
  (define gstat-img (pad (above gstat-lbl gstat-dat) IMG-PAD))
  
  (define event-lbl (text "Last Event" LBL-SIZE LBL-COLOR))
  (define event-dat (let ([event (world-event ws)])
                      (text (~a (cond
                                  [(false? event) ""]
                                  [(eq? event 'clock-tick) "Tme passes"]
                                  [(string=? event "s") "status/score"]
                                  [(string=? event "k") "Kicking"]
                                  [(string=? event "up") "up"]
                                  [(string=? event "down") "down"]
                                  [else event]))
                            DATA-SIZE DATA-COLOR)))
  (define event-img (pad (above event-lbl event-dat) IMG-PAD))

  (define min-hspc4-width 200)
  (define hspc4 (rectangle (max min-hspc4-width
                                (image-width diff?-img)
                                (image-width gstat-img)
                                (image-width event-img))
                           SPC-HEIGHT
                           'solid SPC-COLOR))
  (define col4 (above hspc4
                      diff?-img
                      hspc4
                      gstat-img
                      hspc4
                      event-img
                      hspc4))
  
  (define vspc (rectangle SPC-WIDTH (+ (* 4 (image-height hspc1))
                                       (image-height gstat-img)
                                       (image-height event-img)
                                       (image-height home?-img))
                          'solid SPC-COLOR))
  (define board
    (beside
     vspc
     col1
     vspc
     col2
     vspc
     col3
     vspc
     (rectangle (* 4 (image-width vspc)) (image-height vspc) 'solid 'transparent)
     vspc
     col4
     vspc))
  board)

(define SQ-SIZE 40)
(define SQ-COLOR 'black)
(define SQ (square SQ-SIZE 'solid SQ-COLOR))

(define BO-DIFF 2)
(define BO-SIZE (+ SQ-SIZE (* 2 BO-DIFF)))
(define BO-COLOR 'white)
(define BO-SQ (square BO-SIZE 'solid BO-COLOR))

(define DL-WIDTH (* 3 (quotient BO-SIZE 4)))
(define DL (rectangle DL-WIDTH BO-DIFF 'solid SQ-COLOR))
(define PL-WIDTH (* 3 (- SQ-SIZE DL-WIDTH)))
(define PL-HEIGHT (* 3 BO-DIFF))
(define RB-COLOR 'white)
(define DT-COLOR 'gold)
(define TK-COLOR 'red)

(define (draw-sq row color)
  (define PL
    (rectangle PL-WIDTH
               PL-HEIGHT
               'solid
               color))
  (cond
    [(zero? row)
     (overlay (overlay PL SQ)
              (overlay/align 'middle 'bottom DL BO-SQ))]
    [(< row (sub1 FIELD-DISP-ROWS))
     (overlay (overlay PL SQ)
              (overlay/align 'middle 'top DL
                             (overlay/align 'middle 'bottom DL BO-SQ)))]
    [else
     (overlay (overlay PL SQ)
              (overlay/align 'middle 'top DL BO-SQ))]))

(define get-sq
  (let ([images (make-hash)])
    (λ (row type)
      (define key (list row type))
      (cond
        [(hash-has-key? images key)
         (hash-ref images key)]
        [else (define img (draw-sq row type))
              (hash-set! images key img)
              img]))))

(define (find-sq-pl-color row col players)
  (define pl (findf (λ (pl) (and (= row (player-row pl)) (= col (player-col pl)))) players))
  (cond
    [(false? pl) 'transparent]
    [(running-back? pl) (if (running-back-tackled? pl) TK-COLOR RB-COLOR)]
    [(and (defensive-tackle? pl) (defensive-tackle-nviz? pl)) 'transparent]
    [else DT-COLOR]))

(define END-ZONE (overlay (rectangle (* 3 (quotient BO-SIZE 4))
                                     (- (* 3 BO-SIZE) (* 2 BO-DIFF))
                                     'solid MT-COLOR)
                          (rectangle BO-SIZE
                                     (* 3 BO-SIZE)
                                     'solid BO-COLOR)))

(define BAR (rectangle (* 2 BO-DIFF) BO-SIZE 'solid 'white))
(define MTBAR (rectangle (* 6 BO-DIFF) BO-SIZE 'solid 'transparent))


(define CROSSBAR
  (above
   (rotate 80 BAR)
   (beside MTBAR BAR) 
   (rotate 100 BAR)))

(define LEFT-END-ZONE (beside CROSSBAR END-ZONE))
(define RIGHT-END-ZONE (rotate 180 LEFT-END-ZONE))



(define (draw-field ws)
  (define r1 (world-disp-start-yl ws))
  (define r2 (+ r1 FIELD-DISP-COLS))
  (define cols (if (world-home? ws)
                   (range r1 r2)
                   (reverse (range r1 r2))))
  (apply beside
         (for/list ([col cols])
           (apply above
                  (for/list ([row (range FIELD-DISP-ROWS)])
                    (get-sq row (find-sq-pl-color row col (world-players ws))))))))

(define LBL-INSTRUCT
  (let* ([txt (text "d=toggle difficulty\ts=stats/score\tk=kick\tarrows=move\tr=restart"
                    LBL-SIZE 'black)]
         [img (pad txt 8)])
    (overlay img
             (rectangle (+ 4 MT-WIDTH)
                        (image-height img)
                        'solid 'green))))

(define (render ws)
  (place-image (above
                (draw-status-and-score-board ws)
                (square SQ-SIZE 'solid 'transparent)
                (beside LEFT-END-ZONE (draw-field ws) RIGHT-END-ZONE)
                (square SQ-SIZE 'solid 'transparent)
                LBL-INSTRUCT)
               (quotient (image-width MT) 2)
               (quotient (image-height MT) 2)
               MT))

;; end?: ws -> boolean?
;; Check if the game is over.
(define (end? ws) (gstat? ws GSTAT-GAME-OVER))

(define (render-final ws) (render ws))

(big-bang (new-world)
  (on-tick clock-tick-handler)
  (on-key  key-stroke-handler)
  (to-draw render)
  (stop-when end? render-final)
  (name MT-NAME))