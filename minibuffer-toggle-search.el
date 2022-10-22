;;; minibuffer-toggle-search.el -*- lexical-binding: t; -*-

;; 初始化文本
(defvar minibuffer-toggle-search-initial-string nil)

(defvar minibuffer-toggle-search-command-alist
  '((consult-line :toggle 'consult-line-multi
                  :initial '(region word url))
    (consult-line-multi :toggle 'consult-ripgrep
                        :initial 'symbol)
    (isearch-forward :toggle 'consult-line
                     :thing 'symbol)
    (evil-ex-search-forward :toggle 'consult-line
                            :initial 'word)
    (consult-ripgrep :toggle 'change-directory
                     :prefix "#"
                     :append nil)
    (helpful-function :toggle 'helpful-variable)
    (helpful-variable :toggle 'describe-face)
    (lmm/SPC-SPC :toggle 'helpful-function)))

;; 当前minibuffer 命令
(defvar minibuffer-toggle-search-current-command nil)

;; 默认为可删除默认文本
(defvar minibuffer-toggle-search-default-text t
  "")

(defface minibuffer-toggle-search-default-text-face
  '((t :inherit shadow))
  "Face use minibuffer inserted default text.")

(defun minibuffer-ts-customize-put (cmds prop form)
  "Set property PROP to FORM of commands CMDS."
  (dolist (cmd cmds)
    (cond
     ((functionp cmd)
      (setf (alist-get cmd minibuffer-toggle-search-command-alist)
            ;; (let ((alist (alist-get cmd minibuffer-toggle-search-command-alist)))
            ;;   (when minibuffer-toggle-search-default-text
            ;;     (plist-put alist :default t))
            ;;   (plist-put alist prop form))
            (plist-put (alist-get cmd minibuffer-toggle-search-command-alist) prop form)
            ))
     (t (user-error "%s is neither a command"
                    cmd))))
  nil)

(defmacro minibuffer-ts-customize (&rest args)
  "Set properties of commands or sources.
ARGS is a list of commands or sources followed by the list of keyword-value
pairs."
  (let ((setter))
    (while args
      (let ((cmds (seq-take-while (lambda (x) (not (keywordp x))) args)))
        (setq args (seq-drop-while (lambda (x) (not (keywordp x))) args))
        (while (keywordp (car args))
          (push `(minibuffer-ts-customize-put ',cmds ,(car args) ',(cadr args)) setter)
          (setq args (cddr args)))))
    (macroexp-progn setter)))

(minibuffer-ts-customize
 evil-ex-search-forward
 :default t
 :initial 'symbol
 consult-ripgrep
 :initial "he"
 :prefix "#"
 consult-org-heading
 consult-line
 :toggle 'consult-line-multi
 :default t
 :initial (thing-at-point 'symbol t)
 consult-line-multi
 :toggle 'consult-line-ripgrep
 )

(defun minibuffer-toggle-search (&optional argu)
  "search in current buffer or current directory
      -> search in all buffer or up directory."
  (interactive "P")
  (when (minibufferp)
    (cond
     ((equal 'minibuffer-toggle-search this-command)
      (let* ((alist (mapcar (lambda (x) (with-minibuffer-selected-window
                                         (eval x 'lexical)))
                           (alist-get minibuffer-toggle-search-current-command
                                      minibuffer-toggle-search-command-alist)))
            (string (ignore-errors
                        (buffer-substring-no-properties (minibuffer-prompt-end)
                                                        (point-max))))
            (command (plist-get alist :toggle)))
        ;; (when argu
        ;;   (setq command (cadr minibuffer-toggle-search-current-command))
        ;;   (setq minibuffer-toggle-search-current-command
        ;;         (append (car minibuffer-toggle-search-current-command)
        ;;         (cddr minibuffer-toggle-search-current-command))))
        ;; (cddr '(a b c d))
        (when-let ((command (plist-get alist :toggle)))
          (run-at-time 0 nil (lambda (com)
                               (setq this-command com
                                     minibuffer-toggle-search-current-command com
                                     minibuffer-toggle-search-initial-string string)
                               (call-interactively com))
                       command)
          (minibuffer-quit-recursive-edit)
          )))
     ((stringp minibuffer-toggle-search-initial-string)
      (insert minibuffer-toggle-search-initial-string)
      (setq minibuffer-toggle-search-initial-string nil))
     (t
      (setq minibuffer-toggle-search-initial-string nil)
      (setq minibuffer-toggle-search-current-command this-command)
      (let ((alist (mapcar (lambda (x) (with-minibuffer-selected-window
                                         (eval x 'lexical)))
                           (alist-get this-command
                                      minibuffer-toggle-search-command-alist))))
        (when-let* ((initial (plist-get alist :initial))
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
          (delete-minibuffer-contents)
          (save-excursion
            (insert (propertize str 'face 'shadow)))
          (add-hook 'pre-command-hook 'minibuffer-ts-del-default-contents)
          )))
     )))

;; 获取定义的命令属性
(defun minibuffer-ts-alist-get(&optional cmd)
  ""
  (mapcar (lambda (x) (with-minibuffer-selected-window
                        (eval x 'lexical)))
          (alist-get (or cmd this-command)
                     minibuffer-toggle-search-command-alist)))
;; 插入预定义的初始化文本
(defun minibuffer-ts-initial()
  ;; (setq minibuffer-toggle-search-initial-string nil)
  ;; (setq minibuffer-toggle-search-current-command this-command)
  (when-let* ((alist (minibuffer-ts-alist-get))
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

    (delete-region (+ (length (plist-get alist :prefix)) (minibuffer-prompt-end)) (point-max))

    (if (or (plist-get alist :default)
            (and (not (plist-member alist :default))
                 minibuffer-toggle-search-default-text))
        (save-excursion
          (add-hook 'pre-command-hook 'minibuffer-ts-del-default-contents)
          (insert (propertize str 'face 'minibuffer-toggle-search-default-text-face)))
      (insert str))
    ))
;; 默认文本自动删除
(defun minibuffer-ts-del-default-contents()
  (cond ((member this-command '(self-insert-command
                                yank))
         (remove-hook 'pre-command-hook 'minibuffer-ts-del-default-contents)
         (when (minibufferp)
           (delete-region (point) (point-max))))
        ((member this-command '(previous-line
                                next-line
                                vertico-next
                                vertico-previous)))
        (t
         (remove-hook 'pre-command-hook 'minibuffer-ts-del-default-contents)
         (when (minibufferp)
           (put-text-property (point) (point-max) 'face 'default)))))

(add-hook 'minibuffer-setup-hook 'minibuffer-ts-initial)
(remove-hook 'minibuffer-setup-hook 'minibuffer-toggle-search)
