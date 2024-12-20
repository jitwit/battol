#lang racket

(require racket/async-channel
         (except-in srfi/1 delete)
         net/http-easy

         "irc.rkt"
         "outils.rkt")


;; configuration to log in to twitch
(define *oauth-token*
  (symbol->string
   (with-input-from-file "ringo-token.txt"
     read)))

(define *username*
  "jingo_ringo")

;; network connection to twitch irc network
(define twitch-connection #f)

(define location #f)

;; semaphore to protect state among threads
(define irl-semaphore
  (make-semaphore 1))

;; handle commands and return response text
(define (response-message message)
  (match message
    ((irc-message tags pref "PRIVMSG" `(,where ,what) message-whole)
     (define who
       (cdr (assq 'display-name (irc-message-tags message))))
     (match (string-split what)
       (_ #f))) ;; unrecognized command/not applicable
    (_ #f))) ;; other types of messages

(define (message-tag tag message)
  (assoc tag (irc-message-tags message)))

(define (chatter-count who)
  (let ((response (get (string-append "https://tmi.twitch.tv/group/user/"
                                      (string-downcase who)
                                      "/chatters"))))
    (response-json response)))

;; respond to applicable messages
(define (respond-to-message message)
  (write message) (newline)
  (match message
    ;; respond to commands
    ((irc-message _ _ "PRIVMSG" `(,where ,what)  _)
     (define response
       ;; these are potential commands, so use the semaphore
       (call-with-semaphore irl-semaphore
                            (thunk (response-message message))))
     (when response
       (irc-send-message twitch-connection where response)))
    (_ (void))))

;; connect to twitch and grab connection in twitch-connection parameter
(define (boot)
  (define-values (c ready)
    (irc-connect "irc.chat.twitch.tv"
                 6697
                 *username*
                 *username*
                 *username*
                 #:ssl 'auto
                 #:password (string-append "oauth:" *oauth-token*)))
  (sync ready)
  (set! twitch-connection c)
  (irc-send-command c "CAP REQ" ":twitch.tv/commands")
  (irc-send-command c "CAP REQ" ":twitch.tv/tags")
  (irc-join-channel c (string-append "#" *username*)))

(define (leave-channel channel)
  (irc-part-channel twitch-connection (string-append "#" channel)))

(define (join-channel channel)
  (when (string? location)
    (leave-channel location))
  (set! location channel)
  (irc-join-channel twitch-connection
                    (string-append "#" channel)))

(define (send-message message)
  (irc-send-message twitch-connection (string-append "#" location) message))

(define peepin
  "domina105RINGOPEEPINFROMBLIND")
(define bunny
  "mcknzRingoBunny")
(define cat-jam
  "badche2CatJAM")
(define side-eye
  "badche2RingoSideEye")
(define ringo-bus
  "domina105RINGOBUS")
(define ringo-cute
  "domina105RINGOCUTE")
(define ringo-rip
  "domina105RESTINPEACERINGO")
(define golden-boi
  "domina105RINGOGOLDENBOI")
(define ringolicious
  "domina105RINGOLICIOUS")
(define rolly-boi
  "domina105RINGOROLLYBOIJR")
(define uwu
  "domina105RINGOUWUWUWUUWUWUW")
(define irl-ezwin
  "badche2RINGOLIFEIMITATESART")
(define ezwin
  "domina105RINGOEZWINGETREKT")
(define cooked
  "domina105COOKED")
(define founding-father
  "domina105FOUNDINGFATHER")
(define ahoy
  "domina105CRABAHOY")
(define crab-sad
  "domina105SAD")
(define menacing
  "domina105MENACING")
(define luci-dance
  "luvtinDANCE")
(define ringo-bunny
  "mcknzRingoBunny")
(define rare-boi
  "domina105RARELIMITEDRINGBUNNY")

(define (quick-fire message n)
  (for ((i (iota n)))
    (send-message message)))

(define (triangle-fire message n)
  (define dh 1)
  (define h 1)
  (for ((i (iota n)))
    (send-message (string-join
                   (map (lambda (x) message)
                        (iota h))))
    (cond ((= h 1) (set! dh 1))
          ((= h 4) (set! dh -1)))
    (set! h (+ h dh))))

(define (fire message n dt)
  (for ((i (iota n)))
    (send-message message)
    (sleep dt)))

(define (repeated s)
  (define n (quotient 500 (+ 1 (string-length s))))
  (string-join (make-list n s)))

(define (attack!)
  (let ((msg (repeated rare-boi)))
    (fire msg 30 0.2)))

(define (creb)
  (boot)
  (join-channel "DOMINANTCRAB"))

;; Unicode Character “⠀” (U+2800) --- how diesiraeswe gets whitespace
;; braille empty or something

