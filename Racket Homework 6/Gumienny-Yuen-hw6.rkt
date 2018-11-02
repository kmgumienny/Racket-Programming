;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Yuen-Gumienny-hw6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;; Joseph Yuen jhyuen@wpi.edu
;; Kamil Gumienny kmgumienny@wpi.edu

;;
;; Problem 1
;;

(define-struct ad (political? name duration production-cost national? time-of-day repetitions))
;; an Ad is a (make-ad Boolean String Natural Natural Boolean String Natural)
;; interp:  a television ad, where
;;          political? is true if the ad is a political ad, false otherwise (for product ad)
;;          name is the name of the politician or the product the ad is for
;;          duration is the length of the ad (in seconds)
;;          production-cost is the cost to produce the ad (in thousands of dollars)
;;          national? is true if the ad is to be aired nationally (false if locally)
;;          time-of-day is one of "P" for primetime, "D" for daytime, "O" for off-hour
;;          repetitions is the number of times the ad is to be played

;; Examples
(define A1 (make-ad true "George Washington" 30 15 true "D" 10))
(define A2 (make-ad true "Donald Trump" 60 20 true "P" 50))
(define A3 (make-ad true "Jon Smith" 25 10 false "O" 10))
(define A4 (make-ad false "Goldfish" 30 15 true "D" 15))
(define A5 (make-ad false "LEGO" 45 20 false "O" 10))
(define A6 (make-ad false "Ritz Crackers" 20 10 false "P" 5))

;; a ListOfAd is one of
;;   empty
;;   (cons Ad ListOfAd)

;; Examples
(define SCHED1 (cons A1 (cons A2 (cons A3 empty))))
(define SCHED2 (cons A4 (cons A5 (cons A6 empty))))
(define SCHED3 (cons A1 (cons A4 (cons A3 (cons A5 empty)))))

;; count-political-ads: ListOfAd -> Natural
;; consumes a list of ads and
;; produces the number of ads in the list that are classified as politcal ads

(define (count-political-ads aloa)
  (length (filter ad-political? aloa)))

;; Test Cases
(check-expect (count-political-ads empty) 0)
;; base-case produces 0
(check-expect (count-political-ads SCHED1) 3)
;; all political counts all
(check-expect (count-political-ads SCHED2) 0)
;; all products produces 0
(check-expect (count-political-ads SCHED3) 2)
;; mix of ads with poltical in front and middle produces 2



;;
;; Problem 2
;;

;; primetime-ads: ListOfAd -> ListOfAd
;; consumes a list of ads and
;; produces a list of all the ads airing in primetime

(define (primetime-ads aloa)
  (local [(define (is-primetime? anad)
            (string=? "P" (ad-time-of-day anad)))]
    (filter is-primetime? aloa)))
             
;; Test Cases
(check-expect (primetime-ads empty) empty)
;; base-case produces empty
(check-expect (primetime-ads (cons A2 (cons A6 empty)))
              (cons (make-ad true "Donald Trump" 60 20 true "P" 50) (cons (make-ad false "Ritz Crackers" 20 10 false "P" 5) empty)))
;; list with only primetime ads produces list with all ads
(check-expect (primetime-ads (cons A1 (cons A3 (cons A4 (cons A5 empty))))) empty)
;; list without any primetime ads produces empty list
(check-expect (primetime-ads SCHED1) (cons (make-ad true "Donald Trump" 60 20 true "P" 50) empty))
;; list with mix of ads (primetime and not) produces list with desired ads



;;
;; Problem 3
;;

;; ListOfString is one of:
;;     - empty
;;     - (cons String ListOfString)
;; interp: contains a list of strings

;; politicians-sponsoring-ads: ListOfAd -> ListOfString
;; consumes a list of ads and
;; produces a list of strings that contains the names of politicians who have
;; political ads (may contain duplicate names)

(define (politicians-sponsoring-ads aloa)
  (map ad-name (filter ad-political? aloa)))

;; Test Cases
(check-expect (politicians-sponsoring-ads empty) empty)
;; base-case produces empty list of strings
(check-expect (politicians-sponsoring-ads SCHED1) (cons "George Washington" (cons "Donald Trump" (cons "Jon Smith" empty))))
;; list with only politician ads produces list with all names of the politicians
(check-expect (politicians-sponsoring-ads SCHED2) empty)
;; list with only product ads produces empty list
(check-expect (politicians-sponsoring-ads SCHED3) (cons "George Washington" (cons "Jon Smith" empty)))
;; list with mix of ads (politicians and products) produces list with only names of politicians
(check-expect (politicians-sponsoring-ads (cons (make-ad true "George Washington" 30 15 true "P" 10)
                                                (cons (make-ad true "George Washington" 30 15 true "D" 10)
                                                      (cons (make-ad true "Donald Trump" 60 20 true "P" 50)
                                                            (cons (make-ad true "Jon Smith" 25 10 false "O" 10) empty)))))
              (cons "George Washington" (cons "George Washington" (cons "Donald Trump" (cons "Jon Smith" empty)))))
;; list with two of the same ad produces a list with two of the same name



;;
;; Problem 4
;;

;; cheap-to-produce: ListOfAd Number -> ListOfAd
;; consumes a list of ads and
;; produces a list that contains those ads for which
;; the which the production costs are less than the
;; given amount (in thousands of dollars)

(define (cheap-to-produce aloa cost)
  (local [(define (less-than-cost? anad)
            (< (ad-production-cost anad) cost))]
    (filter less-than-cost? aloa)))

;; Test Cases
(check-expect (cheap-to-produce empty 0) empty)
;; an empty list produces an empty list
(check-expect (cheap-to-produce SCHED1 30) SCHED1)
;; a list with all ads under the specified cost produces all ads
(check-expect (cheap-to-produce SCHED2 10) empty)
;; a list with no ads under the specified cost produces an empty list
(check-expect (cheap-to-produce (list (make-ad true "George Washington" 30 15 true "D" 10)) 15) empty)
;; a list with an ad's production cost as the specified number produces empty
(check-expect (cheap-to-produce SCHED1 20) (list A1 A3))
;; a list with a mix of production costs produces only ads with a production cost below the specified cost


                                 
;;
;; Problem 5
;;

(define-struct user (username messages))
;; a User is (make-user String ListOfMessages)
;; interp: a User is where
;;     - username is the name of the user
;;     - messages is a collection of messages in the user's mailbox

;; ListOfUsers is one of:
;;     - empty
;;     - (cons User ListOfUsers)
;; interp: consists of a list of users

(define-struct message (user text read?))
;; a Message is (make-message User String Boolean)
;; interp: a Message is where
;;    - user is the user who sent the message
;;    - text is the text of the message
;;    - read? is true if the message has been read or not.

;; ListOfMessages is one of:
;;    - empty
;;    - (cons Message ListOfMessages)
;; interp: consists of a list of messages

;; Message Examples
(define M1 (make-message
            (make-user "Bob" empty)
            "Hi Sarah. How are you doing?"
            true))
(define M2 (make-message
            (make-user "Sarah" (list M1))
            "I'm living the dream. I just finished the CS homework."
            false))

;; ListOfMessages Examples
(define MAILBOX1 (list M1 M2))
(define MAILBOX2 (list M1))

;; User Examples
(define BOB (make-user "Bob" (list empty)))
(define SARAH (make-user "Sarah" (list M1)))

;; ListOfUsers Examples
(define mailsys empty)                       



;;
;; Problem 6
;;

;; add-user: String -> void
;; consumes a username and
;; produces a new user with the given username
;; and an empty mailbox
;; assume that the username is not already in the mail system
;; EFFECT: modifies the contents of mailsys

(define (add-user username)
  (set! mailsys (cons (make-user username empty) mailsys)))



;;
;; Problem 7
;;

;; send-email: String String String -> void
;; consumes the name of the sender of an email,
;; the name of the recipient of the email,
;; and the text of an email message, and
;; produces a new unread message in the recipient's mailbox
;; assume the named recipient is a user in the mail system
;; EFFECT: modifies the contents of mailsys

(define (send-email sender recipient text)
  (begin0 (set! mailsys (send mailsys sender recipient text))
          (update-mail-system mailsys)))

;; send: ListOfUser String String String -> ListOfUser
;; consumes a mail system, the name of the sender of an email,
;; the name of the recipient of the email,
;; and the text of an email message, and
;; produces a new unread message in the recipient's mailbox

(define (send alou sender recipient text)
  (cond [(empty? alou) (error "Recipient must exist")]
        [(cons? alou) (if (string=? recipient (user-username (first alou)))
                          (cons (make-user (user-username (first alou))
                                           (cons (make-message (retrieve-user alou sender) text false) (user-messages (first alou))))
                                (rest alou))
                          (cons (first alou)
                                (send (rest alou) sender recipient text)))]))

;; retrieve-user: ListOfUser String -> User
;; consumes a list of users and a username and
;; produces the User with that username

(define (retrieve-user alou username)
  (cond [(empty? alou) (make-user username empty)]
        [(cons? alou) (if (string=? username (user-username (first alou)))
                          (first alou)
                          (retrieve-user (rest alou) username))]))

;; Test Case
(check-expect (retrieve-user empty "Bob") (make-user "Bob" empty))
;; a username with an empty list produces user with a new name
(check-expect (retrieve-user (list (make-user "Sarah" empty) (make-user "George" empty)) "George") (make-user "George" empty))
;; a username in the list produces the desired user
(check-expect (retrieve-user (list (make-user "Sarah" empty) (make-user "George" empty)) "Bob") (make-user "Bob" empty))
;; a username outside of the list produces user with the new name

;; update-mail-system: ListOfUser -> void
;; interp: every time a message goes out, this function runs and 
;; updates all the messages that ever went out with the status of
;; the sender at the current time (updates their mailboxes
;; in every message that was sent by them)
;; EFFECT: For ever message that went out by a user, the sender portion of that email
;; is updated with the user at the time this code runs. Changes every sender section
;; of every email sent. Changes mailsys.

(define (update-mail-system alou)
  (cond [(empty? alou) empty]
        [(cons? alou) (begin
                        (local [(define (go-through-messages amessage)
                                  (set-message-user! amessage (retrieve-user mailsys (user-username (message-user amessage)))))]
                          (map go-through-messages (user-messages (first alou))))
                        (update-mail-system (rest alou)))]))


 
;;
;; Problem 8
;;

;; get-unread-messages: String -> ListOfMessages
;; consumes a username and
;; produces a list containing unread messages of the user with
;; the given username
;; assume the username is a valid user in the mail system
;; EFFECT: all unread messages in the named user's mailbox have been set to read

(define (get-unread-messages username)
  (local [(define (get-unread-messages alou username)
            (cond [(empty? alou) (error "User does not exist")]
                  [(cons? alou) (if (string=? username (user-username (first alou)))
                                    (begin0
                                      (local [(define (unread? message)
                                                (not (message-read? message)))]
                                        (filter unread? (user-messages (first alou))))
                                      (local [(define (set-read amessage)
                                                (set-message-read?! amessage true))]
                                        (map set-read (user-messages (first alou)))))
                                    (get-unread-messages (rest alou) username))]))]
          (get-unread-messages mailsys username)))



;;
;; Problem 9
;;

;; most-messages: -> User
;; produces the user in the mailsystem with the largest number of messages in his/her mailbox
;; if there are no users in the system, the function produces an appropriate error
;; if two or more users have the most messages, the function just needs to return one of them (it doesn't matter which one)

(define (most-messages)
    (local [(define (most-messages alou acc)
              (cond [(empty? alou) acc]
                    [(cons? alou) (if (> (length (user-messages (first alou))) (length (user-messages acc)))
                                      (most-messages (rest alou) (first alou))
                                      (most-messages (rest alou) acc))]))]
     (if (empty? mailsys)
         (error "No users in mailbox")
         (most-messages mailsys (first mailsys)))))



;;
;; Problem 10
;;

"Add users (add-user) for mailsys"
(add-user "Bob")
(add-user "Jose")
(add-user "Sarah")
"Show the contents of mailsys"
mailsys
"Send emails (send-email)"
(send-email "Jose" "Bob" "Hello")
(send-email "Jose" "Bob" "Mello")
(send-email "Bob" "Jose" "Ello")
"Show the contents of mailsys"
mailsys
"Show that email sender fields update after a new message is sent"
(send-email "Bob" "Jose" "Everything updated in Bob's email's sender field.")
mailsys
"Gather unread mail in Bob's inbox that will be read true"
(get-unread-messages "Bob")
"break"
"Show the contents of mailsys"
mailsys
"Shows which user has the most messages in mailsys"
(most-messages)