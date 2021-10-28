(setq package-enable-at-startup nil)
(setq native-comp-deferred-compilation-deny-list
      '("init\\.el$"
        "aj-org-config\\.el$"
        "aj-mail-config\\.el$"
        ;; "pdf-[-a-z]+\\.el$"
        "mu4e-query-helper\\.el$"
        "the-org-mode-expansions\\.el$"))

(setq org-roam-v2-ack t)
