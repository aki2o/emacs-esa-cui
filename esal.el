(require 'log4e)
(require 'yaxception)


(log4e:deflogger "esal" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                  (error . "error")
                                                  (warn  . "warn")
                                                  (info  . "info")
                                                  (debug . "debug")
                                                  (trace . "trace")))
(esal--log-set-level 'trace)


(defgroup esal nil
  ""
  :group 'convenience
  :prefix "esal-")

(defvar esal--process-hash (make-hash-table :test 'equal))
(defvar esal--access-token-hash (make-hash-table :test 'equal))
(defvar esal--current-team nil)
(defvar esal--response "")

(defsubst esal--response-finished-p ()
  (and (not (string= esal--response ""))
       (= (string-to-char (substring esal--response -1)) 3)
       (setq esal--response (substring esal--response 0 -1))
       t))

(defsubst esal--quote-argument (s)
  (format "'%s'" (replace-regexp-in-string "'" "\\'" s)))


(defun esal--receive-response (proc res)
  (esal--trace "Received response.\n%s" res)
  (yaxception:$
    (yaxception:try
      (when (stringp res)
        (setq esal--response (concat esal--response res))))
    (yaxception:catch 'error e
      (esal--error "Failed receive response : %s\n%s"
                      (yaxception:get-text e)
                      (yaxception:get-stack-trace-string e)))))

(defun* esal--request (cmdstr &key async)
  (esal--debug "Start request. cmdstr[%s] async[%s]" cmdstr async)
  (cond (async
         (process-send-string (esal-get-process) (concat cmdstr "\n"))
         t)
        (t
         (esal--get-response cmdstr)
         t)))

(defun* esal--get-response (cmdstr &key waitsec)
  (esal--debug "Start get response. cmdstr[%s] waitsec[%s]" cmdstr waitsec)
  (let ((proc (esal-get-process))
        (waiti 0)
        (maxwaiti (* (or waitsec 1) 5)))
    (setq esal--response "")
    (process-send-string proc (concat cmdstr "\n"))
    (esal--trace "Start wait response from server.")
    (while (and (< waiti maxwaiti)
                (not (esal--response-finished-p)))
      (accept-process-output proc 0.2 nil t)
      (incf waiti))
    (when (not (< waiti maxwaiti))
      (esal--warn "Timeout get response of %s" cmdstr))
    (esal--trace "Got response from server.")
    esal--response))


(defun esal-get-process ()
  (let ((proc (gethash esal--current-team esal--process-hash)))
    (or (and (processp proc)
             (eq (process-status proc) 'run)
             proc)
        (esal-start-process))))

(defun esal-exist-process ()
  (let ((proc (gethash esal--current-team esal--process-hash)))
    (and (processp proc)
         (process-status proc)
         t)))

(defun esal-start-process ()
  (when (not esal--current-team)
    (error "[esal] set active team not yet!"))
  (setq esal--response "")
  (let* ((procnm (format "esal-%s" esal--current-team))
         (access-token (gethash esal--current-team esal--access-token-hash))
         (cmd (format "esal login %s -a %s -non-interactive" esal--current-team access-token))
         (proc (start-process-shell-command procnm nil cmd))
         (waiti 0))
    (set-process-filter proc 'esal--receive-response)
    (case system-type
      ((darwin)
       (set-process-coding-system proc 'utf-8-nfd-dos 'utf-8-nfd-unix))
      (t
       (set-process-coding-system proc 'utf-8-dos 'utf-8-unix)))
    (process-query-on-exit-flag proc)
    (while (and (< waiti 50)
                  (not (esal--response-finished-p)))
        (accept-process-output proc 0.2 nil t)
        (incf waiti))
    (puthash esal--current-team proc esal--process-hash)))

;;;###autoload
(defun esal-stop-process ()
  (interactive)
  (when (esal-exist-process)
    (let ((proc (gethash esal--current-team esal--process-hash)))
      (process-send-string proc "exit\n"))))

(defun esal-exit ()
  (esal--request "exit"))

(defun esal-ls (&optional path)
  (esal--get-response (if path
                             (format "ls %s" (esal--quote-argument path))
                           "ls")))

(defun esal-cd (path)
  (esal--request (format "cd %s" (esal--quote-argument path))))

(defun* esal-cat (path &key json indent)
  (let ((cmd (format "cat %s%s%s"
                     (if json "--json " "")
                     (if indent "" "--noindent ")
                     (esal--quote-argument path))))
    (esal--get-response cmd)))

(defun esal-lock (path)
  (esal--request (format "lock %s" (esal--quote-argument path))))

(defun esal-unlock (path)
  (esal--request (format "unlock %s" (esal--quote-argument path))))

(defun* esal-update (path &key wip ship tags category name message without-body-p lock-keep-p)
  (message
   (esal--get-response (format "update %s%s%s%s%s%s%s%s%s"
                                  (if wip "--wip " "")
                                  (if ship "--ship " "")
                                  (if tags
                                      (mapconcat (lambda (tag) (format "--tag=%s" (esal--quote-argument tag))) tags " ")
                                    "")
                                  (if category
                                      (format "--category=%s " (esal--quote-argument category))
                                    "")
                                  (if name
                                      (format "--name=%s " (esal--quote-argument name))
                                    "")
                                  (if message
                                      (format "--message=%s " (esal--quote-argument message))
                                    "")
                                  (if without-body-p "--nobody " "")
                                  (if lock-keep-p "--keeplock " "")
                                  (esal--quote-argument path))
                          :waitsec 5)))

(defun* esal-regist (path &key wip ship tags category name message)
  (message
   (esal--get-response (format "regist %s%s%s%s%s%s%s"
                                  (if wip "--wip " "")
                                  (if ship "--ship " "")
                                  (if tags
                                      (mapconcat (lambda (tag) (format "--tag=%s" (esal--quote-argument tag))) tags " ")
                                    "")
                                  (if category
                                      (format "--category=%s " (esal--quote-argument category))
                                    "")
                                  (if name
                                      (format "--name=%s " (esal--quote-argument name))
                                    "")
                                  (if message
                                      (format "--message=%s " (esal--quote-argument message))
                                    "")
                                  (esal--quote-argument path))
                          :waitsec 5)))

(defun* esal-sync (targets &key all force by-number)
  (let* ((access-token (gethash esal--current-team esal--access-token-hash))
         (cmd (format "esal sync %s -a %s -q %s%s%s%s"
                      esal--current-team
                      access-token
                      (if all "-a " "")
                      (if force "-f " "")
                      (if by-number "-n " "")
                      (mapconcat 'esal--quote-argument targets " "))))
    (async-shell-command cmd)))


(defun esal-teams ()
  (loop for k being hash-key in esal--access-token-hash collect k))

;;;###autoload
(defun esal-set-active-team (team &optional buffer-local-p)
  (interactive
   (list (completing-read "Team: " (esal-teams) nil t nil '())))
  (if buffer-local-p
      (set (make-local-variable 'esal--current-team) team)
    (setq esal--current-team team)))

;;;###autoload
(defun* esal-regist-team (team &key access-token)
  (puthash team access-token esal--access-token-hash)
  t)


(provide 'esal)
