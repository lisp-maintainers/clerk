(in-package #:cl-user)
(defpackage #:clerk
  (:use #:cl)
  (:export #:*jobs*
           #:empty-jobs-queue
           #:job
           #:job-fn
           #:start
           #:stop
           #:calendar
           #:job-function))
(in-package #:clerk)

(defparameter *jobs* nil
  "All scheduled jobs")
(defparameter *main-thread* nil)

(defclass job ()
  ((name :initarg :name :reader name)
   (delay :initarg :delay :reader delay)
   (fire-time :initarg :fire-time :accessor fire-time)
   (payload :initarg :payload :reader payload)))

(defclass continuous-job (job)
  ((interval :initarg :interval :reader interval)))
(defclass one-time-job (job) ())

(defmethod initialize-instance :after ((job job) &key)
  (let ((fire-time (clerk.time:timejump (get-universal-time)
                                        (delay job))))
    (setf (fire-time job)
          fire-time)))

(defun job-function (func &key name in every after)
  (when (and in after)
    ;;In and after are synonyms
    (error "Only one of :in or :after should be used"))
  ;; If neither in or after is set, we use every for the initial delay
  (let ((delay (or in after every)))
    (add-to-jobs-queue
     (if every
         (make-instance 'continuous-job
                        :name name
                        :delay delay
                        :interval every
                        :payload func)
         (make-instance 'one-time-job
                        :name name
                        :delay delay
                        :payload func)))))

;;Deprecated. Use job-function.
(defun job-fn (name type interval fn)
  (let ((every (when (string= :every type) t)))
    (job-function fn :name name :in (unless every interval) :every (when every interval))))

(defun process-pairs (pairs)
  (unless (evenp (length pairs))
    (error "Need an even number of types and intervals"))
  (unless (< 0 (length pairs))
    (error "No interval provided"))
  (loop for (type interval) on pairs by #'cddr
        collect (cond
                  ((string= :every type) :every)
                  ((string= :in type) :in)
                  ((string= :after type) :after)
                  (t (error "Not a recognized interval type")))
        collect interval))

(defmacro job (name &rest type/interval-pairs-and-body)
  (let ((pairs (process-pairs (butlast type/interval-pairs-and-body)))
        (body (car (last type/interval-pairs-and-body))))
    `(job-function
      (lambda () ,body)
      :name ,name
      :after ',(or (getf pairs :in) (getf pairs :after))
      :every ',(getf pairs :every))))

(defun add-to-jobs-queue (job)
  (push job *jobs*)
  (sort *jobs* #'< :key #'fire-time)
  job)

(defun empty-jobs-queue ()
  (setf *jobs* nil))

(defun fire-job-p (job)
  "Check if it is time to fire a job"
  (<= (fire-time job) (get-universal-time)))

(defmethod fire-job ((job job))
  (bt:make-thread (payload job) :name (name job)))

(defmethod fire-job :before ((job continuous-job))
  "Create the next job in the job queue when firing continuous
jobs."
  (with-slots (name interval payload) job
    (add-to-jobs-queue
     (make-instance 'continuous-job
                    :name name
                    :delay interval
                    :interval interval
                    :payload payload))))

(defun fire-job-if-needed ()
  (when (and *jobs* (fire-job-p (car *jobs*)))
    (fire-job (pop *jobs*))
    ;; just in case the second job in queue is the same
    ;; second as the first one. Or there might be a lot of
    ;; jobs in the queue.
    (fire-job-if-needed)))

(defun start ()
  "Start the thread that waits for a jobs to fire."
  (setf *main-thread*
        (bt:make-thread
         #'(lambda ()
             (loop
                (fire-job-if-needed)
                (sleep 1)))
         :name "Clerk scheduler thread")))

(defun stop ()
  "Stop scheduler"
  (bt:destroy-thread *main-thread*)
  (setf *main-thread* nil))

(defgeneric print-job (job stream))
(defmethod print-job ((job job) stream)
  (with-slots (name delay fire-time) job
    (format stream "~A - ~A - ~A~%" name delay fire-time)))
(defmethod print-job ((job continuous-job) stream)
  (with-slots (name delay interval fire-time) job
    (format stream "~A - ~A ~A - ~A~%" name delay interval fire-time)))

(defun calendar (&optional (stream *standard-output*))
  "Print the scheduled jobs"
  (format stream "JOBS:~%")
  (dolist (job *jobs*)
    (print-job job stream)))

