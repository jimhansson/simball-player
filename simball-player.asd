;;;; simball-player.asd
(asdf:defsystem #:simball-player
  :description "Simball Player API-tjänst med Clack och strategier"
  :author "Ditt Namn"
  :license "MIT"
  :serial t
  :depends-on (#:clack #:clack-router #:jonathan)
  :components ((:file "src/main")))
