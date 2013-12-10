(require 'package)

;; Install el-get 
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
    (with-current-buffer
        (url-retrieve-synchronously
             "http://raw.github.com/dimitri/el-get/master/el-get-install.el")
            (let (el-get-master-branch)
                 (goto-char (point-max))
                 (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

;; Install packages
(add-to-list 'el-get-sources
  '(:name evil          :type elpa   :pkgname "evil")
  '(:name evil-leader   :type elpa   :pkgname "evil-leader")
  '(:name evil-org      
     :type git
     :url "git://github.com/edwtjo/evil-org-mode"
     :load "evil-org.el"
     :compile ("evil-org.el")
     :features evil-org))

(setq my-el-get-packages  
    (append  
     '(evil evil-leader evil-org cyberpunk-theme)  
      (mapcar 'el-get-source-name el-get-sources)))  

(el-get 'sync my-el-get-packages)

(require 'evil-leader)
(require 'evil)
(evil-mode 1)
(require 'evil-org)
