;;;; simball-player.asd

(asdf:defsystem #:simball-player
  :description "Simball Player API-tjänst med Clack och strategier"
  :author "Ditt Namn"
  :license "MIT"
  :serial t
  :depends-on (#:clack #:jonathan #:lack #:lack-response #:lack-request #:cl-ppcre)
  :components ((:file "src/main")))
