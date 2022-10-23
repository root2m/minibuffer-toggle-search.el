;;; minibuffer-toggle-search.el --- minibuffer toggle current search -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(defvar minibuffer-toggle-search-initial-string nil
  "初始化文本.")

;; (defvar minibuffer-toggle-search-command-alist
;;   '((consult-line :toggle 'consult-line-multi
;;                   :initial '(region word url))
;;     (consult-line-multi :toggle 'consult-ripgrep
;;                         :initial 'symbol)
;;     (isearch-forward :toggle 'consult-line
;;                      :thing 'symbol)
;;     (evil-ex-search-forward :toggle 'consult-line
;;                             :initial 'word)
;;     (consult-ripgrep :toggle 'change-directory
;;                      :prefix "#"
;;                      :append nil)
;;     (helpful-function :toggle 'helpful-variable)
;;     (helpful-variable :toggle 'describe-face)
;;     (lmm/SPC-SPC :toggle 'helpful-function)))
(defvar minibuffer-toggle-search-command-alist
  nil
  "Command configuration alist for `minibuffer-toggle-search'.")

;; 当前minibuffer 命令, from `this-command'
(defvar minibuffer-toggle-search-current-command nil
  "最后调用minibuffer 的命令.")

(defvar minibuffer-toggle-search-default-text t
  "默认为可删除默认文本.")

(defface minibuffer-toggle-search-default-text-face
  '((t :inherit shadow))
  "Face use minibuffer inserted default text.")

(defun minibuffer-toggle-search-customize-put (cmds prop form)
  "Set property PROP to FORM of commands CMDS."
  (dolist (cmd cmds)
    (cond
     ((functionp cmd)
      (setf (alist-get cmd minibuffer-toggle-search-command-alist)
            (plist-put (alist-get cmd minibuffer-toggle-search-command-alist) prop form)
            ))
     (t (user-error "%s is neither a command"
                    cmd))))
  nil)

(defmacro minibuffer-toggle-search-customize (&rest args)
  "Set properties of commands or sources.
ARGS is a list of commands or sources followed by the list of keyword-value
pairs.
all keyword:
:toggle toggle to cmd.
:prefix minibuffer default prefix string.
:default shadow text. insert any text to delete default text.
Example: (minibuffer-toggle-search-customize
 execute-extended-command
 :toggle 'helpful-callable
 evil-ex-search-forward
 :toggle 'consult-ripgrep
 :default t
 :initial nil
 :key ?/
 :prefix nil
 consult-grep
 :toggle 'consult-ripgrep
 :key ?g
 :prefix \"#\"
 )."
  (let ((setter))
    (while args
      (let ((cmds (seq-take-while (lambda (x) (not (keywordp x))) args)))
        (setq args (seq-drop-while (lambda (x) (not (keywordp x))) args))
        (while (keywordp (car args))
          (push `(minibuffer-toggle-search-customize-put ',cmds ,(car args) ',(cadr args)) setter)
          (setq args (cddr args)))))
    (macroexp-progn setter)))

;; (minibuffer-toggle-search-customize
;;  execute-extended-command
;;  :toggle 'helpful-callable
;;  evil-ex-search-forward
;;  :toggle 'consult-ripgrep
;;  :default t
;;  :initial nil
;;  :key ?/
;;  :prefix nil
;;  consult-grep
;;  :toggle 'consult-ripgrep
;;  :key ?g
;;  :prefix "#"
;;  consult-ripgrep
;;  :initial nil
;;  :key ?r
;;  :prefix "#"
;;  consult-line-multi
;;  :toggle 'consult-ripgrep
;;  consult-line
;;  :toggle 'consult-ripgrep
;;  :initial 'symbol
;;  :default t
;;  :initial (thing-at-point 'symbol t)
;;  )
;;;###autoload
(defun minibuffer-toggle-search (&optional argu)
  "Search in current buffer or current directory.
toggle to search in all buffer or up directory.
if single argument, goback search."
  (interactive "P")
  (when-let* (((and (minibufferp) (equal 'minibuffer-toggle-search this-command)))
              (alist (minibuffer-toggle-search-alist-get minibuffer-toggle-search-current-command))
              (command (plist-get alist :toggle))
              (string (minibuffer-contents-no-properties)))
    ;; (let ((old-pre (plist-get alist :prefix))
    ;;       (new-pre (plist-get (minibuffer-toggle-search-alist-get command) :prefix)))
    ;;   (cond (old-pre
    ;;          (setq string (replace-regexp-in-string (concat "^" old-pre) (or new-pre "") string)))
    ;;         (new-pre
    ;;          (setq string (concat new-pre string)))))
    (setq string (substring string (length (plist-get alist :prefix))))
    (if (commandp command)
        (run-at-time 0 nil (lambda (com)
                             (setq this-command com
                                   minibuffer-toggle-search-current-command com
                                   minibuffer-toggle-search-initial-string string)
                             (unwind-protect
                                 (command-execute com)
                               (setq this-command nil
                                     minibuffer-toggle-search-current-command nil
                                     minibuffer-toggle-search-initial-string nil)))
                     command)
      (user-error "`%s' not a command. error in alist `%s' :toggle ."
                  command minibuffer-toggle-search-current-command))
    (minibuffer-quit-recursive-edit)))

(defun minibuffer-toggle-search-alist-get(&optional cmd)
  "获取定义的`CMD'属性."
  (with-minibuffer-selected-window
    (mapcar (lambda (x)
              (eval x 'lexical))
            (alist-get (or cmd real-this-command)
                       minibuffer-toggle-search-command-alist))))

;;;###autoload
(defun minibuffer-toggle-search-initial()
  "插入预定义的初始化文本."
  (setq minibuffer-toggle-search-current-command this-command)
  (if-let ((string minibuffer-toggle-search-initial-string))
      (or (not (stringp string)) (setq minibuffer-toggle-search-initial-string nil) (insert string))
    (when-let* ((alist (minibuffer-toggle-search-alist-get))
                (initial (plist-get alist :initial))
                (str (with-minibuffer-selected-window
                       (cond ((functionp initial)
                              (funcall initial))
                             ((stringp initial)
                              initial)
                             ((listp initial)
                              (seq-some (lambda (s)
                                          (thing-at-point s t))
                                        initial))
                             (t
                              (thing-at-point initial t))))))

      (when (plist-get alist :override)
          (delete-region (+ (length (plist-get alist :prefix)) (minibuffer-prompt-end)) (point-max)))

      (if (or (plist-get alist :default)
              (and (not (plist-member alist :default))
                   minibuffer-toggle-search-default-text))
          (save-excursion
            (add-hook 'pre-command-hook 'minibuffer-toggle-search-del-default-contents)
            (insert (propertize str 'face 'minibuffer-toggle-search-default-text-face)))
        (insert str))
      )))

;;;###autoload
(defun minibuffer-toggle-search-del-default-contents()
  "默认文本自动删除."
  (cond ((member this-command '(self-insert-command
                                yank))
         (remove-hook 'pre-command-hook 'minibuffer-toggle-search-del-default-contents)
         (when (minibufferp)
           (delete-region (point) (point-max))))
        ((member this-command '(previous-line
                                next-line
                                vertico-next
                                vertico-previous)))
        (t
         (remove-hook 'pre-command-hook 'minibuffer-toggle-search-del-default-contents)
         (when (minibufferp)
           (put-text-property (point) (point-max) 'face 'default)))))

(add-hook 'minibuffer-setup-hook 'minibuffer-toggle-search-initial 100)
(remove-hook 'minibuffer-setup-hook 'minibuffer-ts-initial)

;;;###autoload
(define-minor-mode minibuffer-toggle-search-mode
  "Insert initialized text in the mini buffer, switch the current search to another search."
  :global t
  (if minibuffer-toggle-search-mode
      (add-hook 'minibuffer-setup-hook 'minibuffer-toggle-search-initial)
    (remove-hook 'minibuffer-setup-hook 'minibuffer-toggle-search-initial)))

(provide 'minibuffer-toggle-search)
;;; minibuffer-toggle-search.el ends here.
