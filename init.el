(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)


(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")
(add-to-list 'load-path "~/.emacs.d/plugins/evil-leader")

(require 'evil-leader)
(require 'evil)
(evil-mode 1)

(require 'evil-org)
