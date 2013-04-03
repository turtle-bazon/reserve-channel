;;;; -*- mode: lisp -*-

(in-package :reserve-channel)

(defparameter *running-process* nil)

(defun pseudo-car (object)
  (if (listp object)
      (car object)
      object))

(defun key-filter (key seq)
  (remove-if-not #'(lambda (elt) (eq elt key)) seq :key #'pseudo-car))

(defun value-filter (key seq)
  (car (cdr (car (key-filter key seq)))))

(defun config-channels (config)
  (key-filter :channel config))

(defun config-test-hosts (config)
  (key-filter :test-host config))

(defun channel-interface (channel)
  (value-filter :interface channel))

(defun channel-gateway (channel)
  (value-filter :gateway channel))

(defun channel-routed (channel)
  (value-filter :routed channel))

(defun channel-purpose (channel)
  (value-filter :purpose channel))

(defun make-route (gateway device metric routed)
  `(,gateway ,device ,metric ,routed))

(defun route-gateway (route)
  (elt route 0))

(defun route-device (route)
  (elt route 1))

(defun route-metric (route)
  (elt route 2))

(defun route-routed (route)
  (elt route 3))

(defun read-config (file)
  (with-open-file (stream file) (read stream)))

(defun test-host-hostname (test-host)
  (value-filter :hostname test-host))

(defun grep (string-list string-part)
  (remove-if
   (complement
    #'(lambda (line)
	(search string-part line)))
   string-list))

(defun to-channels (channel-lines)
  (mapcar
   #'(lambda (channel-line)
       (get-channel channel-line))
   channel-lines))

(defun is-channel-alive (device test-hosts)
  (find-if
   #'(lambda (test-host)
       (= 1 (length 
	     (grep (run/strings 
		    "ping" 
		    `("-c1"
		      "-i0.05"
		      ,(format nil "-I~a" device)
		      ,(test-host-hostname test-host))) 
		   "time="))))
   test-hosts))

(defun alive-channel (config)
  (find-if
    #'(lambda (channel)
        (let ((device (channel-interface channel)))
          (is-channel-alive device (if (channel-routed channel)
                                     (cons (channel-gateway channel) (config-test-hosts config))
                                     (config-test-hosts config)))))
    (config-channels config)))

(defun default-routes ()
  (sort 
   (loop for route-line in (run/strings "/sbin/route" `("-n"))
      for route-strings = (split-sequence #\Space
					  route-line :remove-empty-subseqs t)
      when (equal (car route-strings) "0.0.0.0")
      collect (make-route (elt route-strings 1)
			  (elt route-strings 7)
			  (parse-integer (elt route-strings 4))
			  (equal (elt route-strings 3) "UG")))
   #'< :key #'route-metric))

(defun delete-default-routes (default-routes)
  (loop for route in default-routes
     do (run/strings 
	 "/sbin/route" `("del" "default" 
			       ,@(if (route-routed route)
				     `("gw"  ,(route-gateway route))
				     `("dev" ,(route-device route)))
			       "metric" ,(format nil "~a"
						 (route-metric route))))))

(defun add-default-route (channel metric)
  (run/strings 
   "/sbin/route" `("add" "default"
			 ,@(if (channel-routed channel)
			       `("gw"  ,(channel-gateway channel))
			       `("dev" ,(channel-interface channel)))
			 "metric" ,(format nil "~a" metric))))

(defun add-other-default-routes (other-channels &optional (start-metric 1))
  (if (not (eq other-channels '()))
    (let ((channel (first other-channels)))
      (add-default-route channel start-metric)
      (add-other-default-routes (rest other-channels) (+ start-metric 1)))))

(defun get-time-string (time)
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time time)
    (format nil "~2,'0d.~2,'0d.~a ~2,'0d:~2,'0d" day month year hour min)))

(defun is-running ()
  *running-process*)

(defun check-channel-logic (config)
  (let* ((alive-channel (alive-channel config))
	 (other-channels (remove alive-channel (config-channels config) 
				 :test #'equal))
	 (default-routes (default-routes)))
    (if (not (eq alive-channel nil))
	(let ((new-route (channel-gateway alive-channel)))
	  (if (not (and (> (length default-routes) 0)
			(equal (car (first default-routes)) new-route)))
	      (progn
		(format t "~a Route changed from ~a to ~a.~%" 
			(get-time-string (get-universal-time))
			default-routes new-route)
		(delete-default-routes default-routes)
		(add-default-route alive-channel 0)
		(add-other-default-routes other-channels)))))))

(defun check-channel (config-location)
  (if (not *running-process*)
      (progn
	(setf *running-process* t)
	(handler-case
	    (check-channel-logic (read-config config-location))
	  (t (e) (format t "~a~&" e)))
	(setf *running-process* nil))))
