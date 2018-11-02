;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname gumienny-yuen-hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Joseph Yuen jhyuen@wpi.edu
;; Kamil Gumienny kmgumienny@wpi.edu

;;
;; Problem 1
;;

;; A Date is (make-date Natural Natural Natural)
(define-struct date (year month day))
;; interpretation: a date is where
;;    year is the year of release
;;    month is the month of release
;;    day is the day of release

;; Example of Date
(define F1D (make-date 2010 6 18))
(define F2D (make-date 2012 5 4))
(define F3D (make-date 2016 5 6))

;; A Film is (make-film String String String Natural Date Natural)
(define-struct film (title genre rating running-time opening-date receipts-collected))
;; interpretation: a film is where
;;    title is the title of the film
;;    genre is the film's genre (drama, comedy, family, etc.)
;;    rating is the film's rating. A rating can be one of G, PG, PG-13, R, NC-17, NR.
;;    running-time is the running time of the film, in minutes
;;    opening-date is the date of the film opened at the theater (it should include the year, month, and day)
;;    receipts-collected is the total box office receipts collected so far for the film (in millions of dollars)

;; Examples of Films
(define F1 (make-film "Toy Story" "Animation" "G" 103 (make-date 2010 6 18) 1067))
(define F2 (make-film "Avengers" "Action" "PG-13" 144 (make-date 2012 5 4) 1520))
(define F3 (make-film "Captain America: Civil War" "Action" "R" 147 (make-date 2016 5 6) 1153))

;;
;; Problem 2
;;

;; Film Signatures:
;; make-film: String String String Natural Date Natural -> Film
;; film-title: Film -> String
;; film-genre: Film -> String
;; film-rating: Film -> String
;; film-running-time: Film -> Natural
;; film-opening-date: Film -> Date
;; film-receipts-collected: Film -> Natural

;;
;; Problem 3
;;

;; suitable-for-children?: Film -> Boolean
;; consumes a film and
;; returns true if the rating of the film is
;; G, PG, or PG-13, and returns false otherwise

(define (suitable-for-children? a-film)
  (cond [(string=? (film-rating a-film) "G" ) true]
        [(string=? (film-rating a-film) "PG" ) true]
        [(string=? (film-rating a-film) "PG-13" ) true]
        [else false]))

;; test cases
(check-expect (suitable-for-children? (make-film "Toy Story" "Animation" "G" 103 (make-date 2010 6 18) 1067)) true)
;; a G rated movie should return true
(check-expect (suitable-for-children? (make-film "Shrek" "Animation" "PG" 103 (make-date 2010 6 18) 1067)) true)
;; a PG rated movie should return true
(check-expect (suitable-for-children? (make-film "Avengers" "Action" "PG-13" 103 (make-date 2010 6 18) 1067)) true)
;; a PG-13 rated movie should return true
(check-expect (suitable-for-children? (make-film "Brave Heart" "Adventure" "R" 103 (make-date 2010 6 18) 1067)) false)
;; a R rated movie should return false
(check-expect (suitable-for-children? (make-film "Scary Movie" "Comedy" "NC-17" 103 (make-date 2010 6 18) 1067)) false)
;; a NC-17 rated movie should return false
(check-expect (suitable-for-children? (make-film "Artsy" "Indie" "NR" 103 (make-date 2010 6 18) 1067)) false)
;; a NR rated movie should return false

;;
;; Problem 4
;;

;; difference-in-receipts: Film Film -> Natural
;; consumes 2 films and
;; produces the difference between the box office
;; receipts for the two films (non-negative)

(define (difference-in-receipts a-film another-film)
  (if (< (norm-diff-receipts a-film another-film) 0)
      (* (norm-diff-receipts a-film another-film) -1)
      (norm-diff-receipts a-film another-film)))

;; test-cases
(check-expect (difference-in-receipts
              (make-film "Avengers" "Action" "PG-13" 144 (make-date 2012 5 4) 1520)
              (make-film "Captain America: Civil War" "Action" "PG-13" 147 (make-date 2016 5 6) 1153))
              367)
;; The subtraction of the first and second movie produces a positive answer
(check-expect (difference-in-receipts
              (make-film "Avengers" "Action" "PG-13" 144 (make-date 2012 5 4) 1100)
              (make-film "Captain America: Civil War" "Action" "PG-13" 147 (make-date 2016 5 6) 1200))
              100)
;; The subtraction of the second and first movie produces a positive answer
(check-expect (difference-in-receipts
              (make-film "Avengers" "Action" "PG-13" 144 (make-date 2012 5 4) 1500)
              (make-film "Captain America: Civil War" "Action" "PG-13" 147 (make-date 2016 5 6) 1500))
              0)
;; The subtraction of the first and second as well as the second and first may produce zero

;; Helper Function
;; norm-diff-receipts: Film Film -> Number
;; consumes a film's receipt and another film's receipt and
;; produces the difference

(define (norm-diff-receipts a-film another-film)
  (- (film-receipts-collected a-film) (film-receipts-collected another-film)))

;; test-cases
(check-expect (norm-diff-receipts
              (make-film "Avengers" "Action" "PG-13" 144 (make-date 2012 5 4) 1520)
              (make-film "Captain America: Civil War" "Action" "PG-13" 147 (make-date 2016 5 6) 1153))
              367)
;; The subtraction of the first and second movie produces a positive answer
(check-expect (norm-diff-receipts
              (make-film "Avengers" "Action" "PG-13" 144 (make-date 2012 5 4) 1100)
              (make-film "Captain America: Civil War" "Action" "PG-13" 147 (make-date 2016 5 6) 1200))
              -100)
;; The subtraction of the first and second movie produces a negative answer
(check-expect (norm-diff-receipts
              (make-film "Avengers" "Action" "PG-13" 144 (make-date 2012 5 4) 1500)
              (make-film "Captain America: Civil War" "Action" "PG-13" 147 (make-date 2016 5 6) 1500))
              0)
;; The subtraction of the first and second movie produces zero

;;
;; Problem 5
;;

;; modify-rating: Film String -> Film
;; consumes a film and the new rating and
;; produces a film that is the same as the original
;; except that the film's rating has been replaced by the given rating

(define (modify-rating a-film new-rating)
  (make-film (film-title a-film) (film-genre a-film) new-rating (film-running-time a-film) (film-opening-date a-film) (film-receipts-collected a-film)))

;; test cases
(check-expect (modify-rating (make-film "Captain America: Civil War" "Action" "G" 147 (make-date 2016 5 6) 1153)
                             "PG")
              (make-film "Captain America: Civil War" "Action" "PG" 147 (make-date 2016 5 6) 1153))
;; Switch from G to PG
(check-expect (modify-rating (make-film "Captain America: Civil War" "Action" "PG" 147 (make-date 2016 5 6) 1153)
                             "PG-13")
              (make-film "Captain America: Civil War" "Action" "PG-13" 147 (make-date 2016 5 6) 1153))
;; Switch from PG to PG-13
(check-expect (modify-rating (make-film "Captain America: Civil War" "Action" "PG-13" 147 (make-date 2016 5 6) 1153)
                             "R")
              (make-film "Captain America: Civil War" "Action" "R" 147 (make-date 2016 5 6) 1153))
;; Switch from PG-13 to R
(check-expect (modify-rating (make-film "Captain America: Civil War" "Action" "R" 147 (make-date 2016 5 6) 1153)
                             "NC-17")
              (make-film "Captain America: Civil War" "Action" "NC-17" 147 (make-date 2016 5 6) 1153))
;; Switch from R to NC-17
(check-expect (modify-rating (make-film "Captain America: Civil War" "Action" "NC-17" 147 (make-date 2016 5 6) 1153)
                             "NR")
              (make-film "Captain America: Civil War" "Action" "NR" 147 (make-date 2016 5 6) 1153))
;; Switch from NC-17 to NR
(check-expect (modify-rating (make-film "Captain America: Civil War" "Action" "NR" 147 (make-date 2016 5 6) 1153)
                             "G")
              (make-film "Captain America: Civil War" "Action" "G" 147 (make-date 2016 5 6) 1153))
;; Switch from NR to G

;;
;; Problem 6
;;

;; opens-before?: Film Date -> Boolean
;; consumes a film and date and
;; produces true if the given film opens before the given date,
;; otherwise returns false 

(define (opens-before? a-film a-date)
  (cond [(< (date-year (film-opening-date a-film)) (date-year a-date)) true]
        [(> (date-year (film-opening-date a-film)) (date-year a-date)) false]
        [(and (year-equal? a-film a-date)
              (< (date-month (film-opening-date a-film)) (date-month a-date))) true]
        [(and (year-equal? a-film a-date)
              (> (date-month (film-opening-date a-film)) (date-month a-date))) false]
        [(and (year-equal? a-film a-date)
              (month-equal? a-film a-date)
              (< (date-day (film-opening-date a-film)) (date-day a-date))) true]
        [(and (year-equal? a-film a-date)
              (month-equal? a-film a-date)
              (>= (date-day (film-opening-date a-film)) (date-day a-date))) false]))

;; test cases
(check-expect (opens-before? (make-film "Captain America: Civil War" "Action" "NR" 147 (make-date 2016 5 6) 1153) (make-date 2016 5 6)) false)
;; equal dates produce false
(check-expect (opens-before? (make-film "Captain America: Civil War" "Action" "NR" 147 (make-date 2016 5 6) 1153) (make-date 2017 5 6)) true)
;; a-film year is before a-date year produce true
(check-expect (opens-before? (make-film "Captain America: Civil War" "Action" "NR" 147 (make-date 2017 5 6) 1153) (make-date 2016 5 6)) false)
;; a-film year is after a-date year produce false
(check-expect (opens-before? (make-film "Captain America: Civil War" "Action" "NR" 147 (make-date 2016 1 6) 1153) (make-date 2016 5 6)) true)
;; a-film year and a-date year are equal AND a-film month is before a-date month produce true
(check-expect (opens-before? (make-film "Captain America: Civil War" "Action" "NR" 147 (make-date 2016 5 6) 1153) (make-date 2016 1 6)) false)
;; a-film year and a-date year are equal AND a-film month is after a-date month produce false
(check-expect (opens-before? (make-film "Captain America: Civil War" "Action" "NR" 147 (make-date 2016 5 1) 1153) (make-date 2016 5 6)) true)
;; a-film year & month and a-date year & month are equal AND a-film day is before a-date month produce true
(check-expect (opens-before? (make-film "Captain America: Civil War" "Action" "NR" 147 (make-date 2016 5 6) 1153) (make-date 2016 5 1)) false)
;; a-film year & month and a-date year & month are equal AND a-film day is after a-date month produce false

;; Helper Functions
;; year-equal?: Film Film -> Boolean
;; consumes a 2 films' years and
;; produces true if the years are equal
;; and returns false otherwise

(define (year-equal? a-film a-date)
  (= (date-year (film-opening-date a-film)) (date-year a-date)))

(check-expect (year-equal? (make-film "Captain America: Civil War" "Action" "NR" 147 (make-date 2016 5 6) 1153) (make-date 2016 5 6)) true)
;; equal years produce true
(check-expect (year-equal? (make-film "Captain America: Civil War" "Action" "NR" 147 (make-date 2016 5 6) 1153) (make-date 2015 5 6)) false)
;; date-year is greater than film-year produce false
(check-expect (year-equal? (make-film "Captain America: Civil War" "Action" "NR" 147 (make-date 2015 5 6) 1153) (make-date 2016 5 6)) false)
;; film-year is greater than date-year produce false

;; month-equal?: Film Film -> Boolean
;; consumes a 2 films' months and
;; produces true if the months are equal
;; and returns false otherwise

(define (month-equal? a-film a-date)
  (= (date-month (film-opening-date a-film)) (date-month a-date)))

(check-expect (month-equal? (make-film "Captain America: Civil War" "Action" "NR" 147 (make-date 2016 5 6) 1153) (make-date 2016 5 6)) true)
;; equal months produce true
(check-expect (month-equal? (make-film "Captain America: Civil War" "Action" "NR" 147 (make-date 2015 5 6) 1153) (make-date 2015 6 6)) false)
;; date-month is greater than film-month produce false
(check-expect (month-equal? (make-film "Captain America: Civil War" "Action" "NR" 147 (make-date 2015 6 6) 1153) (make-date 2015 5 6)) false)
;; film-month is greater than date-month produce false