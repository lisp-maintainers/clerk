(in-package #:cl-user)
(defpackage #:clerk.time
  (:use #:cl)
  (:export #:timejump))
(in-package #:clerk.time)

(defparameter *days-of-the-week* '(monday tuesday wednesday thursday
                                   friday saturday sunday))

(defun split-interval (interval)
  (destructuring-bind (n interval-type)
      (cl-ppcre:split #\. (string interval))
    (values (parse-integer n)
            (intern interval-type))))

(defun interval-type->seconds (interval-type)
  (cdr (assoc interval-type '((second . 1)
                              (seconds . 1)
                              (minute . 60)
                              (minutes . 60)
                              (hour . 3600)
                              (hours . 3600)
                              (day . 86400)
                              (days . 86400)
                              (week . 604800)
                              (weeks . 604800)
                              (month . 2419200)
                              (months . 2419200)
                              ;; years are (* days 365)
                              ;; regardless if the current year is
                              ;; a leap year
                              (year . 31536000)
                              (years . 31536000))
              :test #'string=)))

(defun day-of-the-week-p (interval)
  (member interval *days-of-the-week* :test #'string=))

(defun day-as-number (day-as-symbol)
  (position day-as-symbol *days-of-the-week* :test #'string=))

(defun current-day-of-the-week ()
  "Returns the current day of the week as an integer. Monday is 0."
  (nth-value 6
             (decode-universal-time
              (get-universal-time))))

(defun days-to-add (current target)
  "Calculates how far are the current day of the week to the target
day of the week."
  (if (< current target)
      (- target current)
      (- 7 (- current target))))

(defun seconds-to-end-of-the-day (current)
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time (+ current 86400))
    (declare (ignore seconds minutes hours))
    (- (encode-universal-time 0 0 0 date month year) current)))

(defun seconds-to-day-of-the-week (day-of-the-week)
  "Returns the seconds to the day of the week"
  (+ (seconds-to-end-of-the-day (get-universal-time))
     (* 86400
        (1- (days-to-add (current-day-of-the-week) (day-as-number day-of-the-week))))))

(defun interval-as-list-p (interval)
  "Check if an interval is given as a list"
  (consp interval))

(defun interval-as-list (interval)
  (* (car interval) (interval-type->seconds (cadr interval))))

(defun interval->seconds (interval)
  (if (interval-as-list-p interval)
      (interval-as-list interval)
      (cond ((day-of-the-week-p interval)
             (seconds-to-day-of-the-week interval))
            (t (multiple-value-bind (n interval-type)
                   (split-interval interval)
                 (* n
                    (interval-type->seconds interval-type)))))))
  
(defun timejump (start-time interval)
  (+ start-time (interval->seconds interval)))
