(defparameter *config-location* "/etc/reserve-channel.conf")

(defstruct channel
  device
  gateway)

(defstruct config
  channels
  test-hosts)

(defun string-split (string &key (ws '(#\Space #\Tab)) (empty-strings t))
  (if (eq string nil) '()
    (let* ((ws-position (position-if #'(lambda (char) (member char ws)) string))
           (string-first (subseq string 0 ws-position))
           (string-rest (cond (ws-position (subseq string (+ ws-position 1)))
                              (t nil))))
      (macrolet ((string-split-req () `(string-split string-rest
                                                     :ws ws
                                                     :empty-strings empty-strings)))

      (if (and (equal string-first "") (not empty-strings))
        (string-split-req)
        (cons string-first (string-split-req)))))))

(defun read-file-to-line-list (file &optional (empty-lines t))
  (with-open-file (fstream file)
    (remove 'eof
            (do* ((line (read-line fstream nil 'eof) (read-line fstream nil 'eof))
                  (book `(,line) (if (and (equal line "") (not empty-lines))
                                   book
                                   (cons line book))))
                 ((eq line 'eof) (reverse book))))))

(defun grep (string-list string-part)
  (remove-if
    (complement
      #'(lambda (line)
	  (search string-part line)))
    string-list))

(defun get-channel (channel-string)
  (let ((split-channel (string-split channel-string :ws '(#\:))))
    (make-channel
      :device (elt split-channel 1)
      :gateway (elt split-channel 2))))

(defun to-channels (channel-lines)
  (mapcar
    #'(lambda (channel-line)
	(get-channel channel-line))
    channel-lines))

(defun read-config (file)
  (let* ((config-lines (read-file-to-line-list file nil))
	 (channel-lines (grep config-lines "channel:"))
	 (test-host-lines (grep config-lines "test-host:")))
    (make-config
      :channels (to-channels channel-lines)
      :test-hosts (mapcar
		    #'(lambda (test-host-line)
			(elt (string-split test-host-line :ws '(#\:)) 1))
		    test-host-lines))))

(defun is-channel-alive (device test-hosts)
  (find-if
    #'(lambda (test-host)
	(= 1 (length 
	       (grep (external-program:run/strings 
		       "ping" 
		       `("-c1" "-i0.05" ,(format nil "-I~a" device) ,test-host)) 
		     "time="))))
    test-hosts))

(defun alive-channel (config)
  (find-if
    #'(lambda (channel)
	(let ((device (channel-device channel)))
	  (is-channel-alive device (config-test-hosts config))))
    (config-channels config)))

(defun default-routes ()
  (sort
    (mapcar
      #'(lambda (route-line)
	  (let ((route-strings (string-split route-line
				 :ws '(#\Space)
				 :empty-strings nil)))
	    `(,(elt route-strings 1) . ,(parse-integer (elt route-strings 4)))))
      (grep (grep (external-program:run/strings "/sbin/route" 
						`("-n")) "UG") "0.0.0.0"))
    #'< :key #'cdr))

(defun delete-default-routes (default-routes)
  (mapcar
    #'(lambda (route)
	(external-program:run/strings 
	  "/sbin/route" `("del" "default" 
			  "gw" ,(car route) 
			  "metric" ,(format nil "~a" (cdr route)))))
    default-routes))

(defun add-default-route (channel metric)
  (external-program:run/strings 
    "/sbin/route" `("add" "default" 
		    "gw" ,(channel-gateway channel) 
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

(let ((running (> (length (grep 
			    (grep
			      (external-program:run/strings "ps" '("aux"))
			      "reserve-channel") "sbcl ")) 1)))
  (if (not running)
    (let* ((config (read-config *config-location*))
	   (alive-channel (alive-channel config))
	   (other-channels (remove alive-channel (config-channels config) 
				   :test #'equal))
	   (default-routes (default-routes)))
      (if (eq alive-channel nil)
	'()
	(let ((new-route (channel-gateway alive-channel)))
	  (if (not (and (> (length default-routes) 0)
			(equal (car (first default-routes)) new-route)))
	    (progn
	      (format t "~a Route changed from ~a to ~a.~%" 
		      (get-time-string (get-universal-time))
		      default-routes new-route)
	      (delete-default-routes default-routes)
	      (add-default-route alive-channel 0)
	      (add-other-default-routes other-channels))))))))
