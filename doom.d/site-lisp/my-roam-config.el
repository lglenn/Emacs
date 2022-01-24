;;; Org-roam


(use-package! org-roam
   :init
   (setq org-roam-v2-ack t))

(after! org-roam

  (load! "local-org-config" doom-private-dir)

  (setq org-roam-directory (file-truename roam-directory))

  (setq org-roam-completion-everywhere t)

  (map! "C-c n l" #'org-roam-buffer-toggle
        "C-c n f" #'org-roam-node-find
        "C-c n g" #'org-roam-graph
        "C-c n i" #'org-roam-node-insert
        "C-c n c" #'org-roam-capture
        "C-c n j" #'org-roam-dailies-capture-today
        "C-M-]"   #'completion-at-point)

  (org-roam-db-autosync-enable)

  (require 'org-roam-protocol)

  ;; Set a nice side buffer view
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))

  (setq org-roam-node-display-template
        (concat "${title:80} " (propertize "${tags:20}" 'face 'org-tag))
        org-roam-node-annotation-function
        (lambda (node) (marginalia--time (org-roam-node-file-mtime node))))

  (setq org-roam-capture-templates '(("d" "default" plain "%?"
                                     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
                                     :unnarrowed t)
                                    ("g" "glossary" plain "%?"
                                     :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: glossary\n")
                                     :unnarrowed t))))

(provide 'my-roam-config)
