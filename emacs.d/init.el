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
   '(:name evil-leader   :type elpa   :pkgname "evil-leader"))

(setq my-el-get-packages  
    (append  
     '(evil evil-leader evil-org cyberpunk-theme)  
      (mapcar 'el-get-source-name el-get-sources)))  

(el-get 'sync my-el-get-packages)

(setq evil-default-cursor t)
(set-cursor-color "#FFFFFF")

;; disable some of the crap on startup
(setq inhibit-startup-message t)
(if (display-graphic-p)
    (tool-bar-mode -1))

(require 'evil-leader)
(require 'evil)
(require 'evil-org)

;; enable evil-mode
(evil-mode 1)

;; rebind tab for org-mode to fix console emacs bug
(evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)
