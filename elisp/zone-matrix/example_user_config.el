(add-to-list 'load-path "~/.emacs.d/elisp/zone-matrix")
(require 'zone-matrix)
(require 'zone-matrix-settings)
(require 'zone-settings)

(setq zone-programs [zone-matrix])
(zone-when-idle 60)
