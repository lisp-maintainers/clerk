# Clerk

A cron-like scheduler with sane DSL

## Example usage

### Job MACRO

```
(job "Say 'Hi' all the time" every 5.seconds (print "Hi"))

(job "Compose and send monthly report"
     every 1.month (send-report (compose-monthly-report)))
```

If you want to see it with your own eyes, make sure to load the following code:

```
(defun write-to-file (msg file)
  (with-open-file (log file
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format log "~A~%" msg)))

(job "Print farbe" every 3.seconds (write-to-file "Farbe" "log.txt"))
(job "Print colour" every 2.seconds (write-to-file "Colour" "log.txt"))
(job "Print @@@@ 1 min @@@@@" every 1.minute
       (write-to-file "@@@@@@ 1 min @@@@@@" "log.txt"))
```
Now, after `(clerk:start)`, tailing `log.txt` should give you something like this:

```
Colour
Farbe
Colour
Colour
Farbe
Colour
Farbe
Colour
Colour
Farbe
Colour
/one minute later.../
@@@@@@ 1 min @@@@@@
/etc.../
```

### Job FUNCTION

The original idea was for users to use the library to execute some sort of execution of a configuration file. However you can use the job creation process programatically with the underlying function `job-function`. E.g.:

```
(defparameter *query-interval* 5)
(job-function
  #'query-api-fn
  :name (format nil "Query the API every ~A seconds" *query-interval*)
  :every `(,*query-interval* seconds))
```

As you can see, you have to provide a function (either anonymous function or a function symbol) as the last argument.

## Installation and usage

Clerk is available in quicklisp: run `(ql:quicklisp 'clerk)` in your REPL to install.

Make sure your jobs are loaded before executing `(clerk:start)`. The jobs reside inside `clerk:*jobs*`, but you can also type `(clerk:calendar)` to see a list of all scheduled and running jobs. 

### Job types

There are two types of jobs - `continuous` and `one-time`. If a job has the keyword `every` after the job description - the job will be continuous. This means that when an event is fired, a new event will be pushed in the event queue for firing exactly `interval` time from now. The jobs above are an example of `continuous` jobs.

A `one-time` job is fired once and then it is removed from the jobs queue. An example of a one-time job can be:

```
(job "Extraordinary event" in 5.days (send-mail "Don't forget X"))
```

Sometimes with a continuous job you want an initial delay that is different than the repetition interval:

``` 
(job "Do it lots, after a while" after 3.days every 2.seconds (send-marketing-email))
```

`after` can be used interchangeably with `in`.

### Intervals

Right now (more are coming soon) there are 2 type of intervals:

1) **Numbered intervals**

```
[number].[interval-type]
```

where the number is a positive integer and the `interval-type` if one of the following: `second`, `minute`, `hour`, `day`, `week`, `month`, `year`. Also you can use the plural form of all these words. For example `1.second` and `2.seconds` are both valid.

2) **Days of the week**

```
(job "Weekly report" every monday (create-report))
```

Pretty self-explanatory. The idea is that if you type the day of the week, clerk will calculate when it is and add an event to the queue.

## Maintenance

Clerk was written by [Petko Tsikov](https://github.com/tsikov/clerk)

It is currently maintained by Ben McGunigle (bnmcgn at gmail dot com) and the team at lisp-maintainers.
