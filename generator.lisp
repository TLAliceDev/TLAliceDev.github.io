(ql:quickload :uiop)

(defparameter *template* "template.html")
(defvar *folder* ".\/")
(defvar *css-path* "./css/style.css")
(defvar *index-path* "index.html")
(defvar *devlogs-path* "devlogs/devlogs.html")

(defun bold (&rest text)
  (concatenate 'string "<b>" (apply #'concatenate 'string text) "</b>"))

(defun italics (&rest text)
  (concatenate 'string "<i>" (apply #'concatenate 'string text) "</i>"))

(defun link (url &rest text)
  (concatenate 'string "<a href=\"" url "\">" (apply #'concatenate 'string text) "</a>"))

(defun heading (level &rest text)
  (concatenate 'string "<h" (write-to-string level) ">" (apply #'concatenate 'string text) "</h" (write-to-string level) ">"))

(defun paragraph (&rest text)
  (concatenate 'string "<p>" (apply #'concatenate 'string text) "</p>"))

(defun quot (&rest text)
  (concatenate 'string "<p style=\"quote\">" (apply #'concatenate 'string text) "</p>"))

(defun li (&rest text)
  (concatenate 'string "<li>" (apply #'concatenate 'string text) "</li>"))

(defun u-list (&rest elements)
  (concatenate 'string "<ul>" (apply #'concatenate 'string (mapcar #'li elements)) "</ul>"))

(defun o-list (&rest elements)
  (concatenate 'string "<ol>" (apply #'concatenate 'string (mapcar #'li elements)) "</ol>"))

(defun line-break ()
  "<br>")

(defun code (&rest text)
  (concatenate 'string "<p style=\"quote\">" (apply #'concatenate 'string text) "</p>"))

(defun image (image &rest alt)
  (concatenate 'string "<img src=\"" image "\" alt=" (apply #'concatenate 'string alt) "></img>"))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
      for old-pos = 0 then (+ pos part-length)
      for pos = (search part string
                :start2 old-pos
                :test test)
      do (write-string string out
               :start old-pos
               :end (or pos (length string)))
      when pos do (write-string replacement out)
      while pos))) 


(defun generate (title content)
  (let ((post (uiop:read-file-string *template*)))
    (setf post (replace-all post "$$TITLE$$" title))
    (setf post (replace-all post "$$CONTENT$$" content))
    (setf post (replace-all post "$$CSSPATH$$" *css-path*))
    (setf post (replace-all post "$$INDEXPATH$$" *index-path*))
    (setf post (replace-all post "$$DEVLOGSPATH$$" *devlogs-path*))
    post))

(defun format-file-name (title)
  (concatenate 'string (substitute #\_ #\Space title) ".html"))

(defun create-listing (title content)
  (list 
    (link (format-file-name title) (heading 2 title))
    (paragraph (subseq content 0 128) "...")))

(defun create-post (title &rest content)
  (let ((f-content (apply 'concatenate 'string (mapcar #'paragraph content))))
    (with-open-file
      (stream (merge-pathnames (format-file-name title) *folder*)
        :direction :output
        :if-exists :supersede
        :if-does-not-exist :create)
      (format stream (generate title f-content)))
    (create-listing title f-content)))

