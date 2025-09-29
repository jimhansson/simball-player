;; start-simball.lisp
;; Startar simball-player utan att behöva pilla med ASDF/Quicklisp-paths manuellt

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))

;; Lägg till projektmappen i Quicklisp local-projects om den inte redan finns
(let ((projdir (truename ".")))
  (unless (member projdir ql:*local-project-directories* :test #'equal)
    (push projdir ql:*local-project-directories*)))

(ql:quickload :simball-player)

(format t "~%simball-player är laddat!~%")
