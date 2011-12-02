
(progn
  (require 'asdf)
  (require 'newscluster)
  (require 'sb-aclrepl)
  (save-lisp-and-die #p"newscluster.core"))