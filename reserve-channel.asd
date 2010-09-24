;;;; -*- mode: lisp -*-

(defsystem :reserve-channel
  :name "reserve-channel"
  :author "Azamat S. Kalimoulline <turtle@bazon.ru>"
  :licence "Lessor Lisp General Public License"
  :version "0.0.1.0"
  :description "Channel reserve (pinger script)"
  :depends-on (external-program-extender)
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "reserve-channel" :depends-on ("package"))))))
