;;; gptel-forge-prs.el --- Generate PR descriptions for forge using gptel -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Authors
;; SPDX-License-Identifier: GPL-3.0

;; Author: Arthur Heymans
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (magit "4.0")(forge "0.3") (gptel "0.9"))
;; Keywords: forge, vc, convenience, llm, pull-request
;; URL: https://github.com/ArthurHeymans/gptel-forge-prs

;;; Commentary:

;; This package uses the gptel library to add LLM integration into
;; forge.  Currently, it adds functionality for generating pull-request
;; descriptions when creating PRs via `forge-create-pullreq'.
;;
;; Features:
;; - Generate PR descriptions based on git diffs between branches
;; - Automatically uses existing buffer content (e.g., PR templates) as structure
;; - Optional rationale input for better context
;; - Customizable prompts and templates
;;
;; Usage:
;; 1. Call `gptel-forge-prs-install' to set up keybindings
;; 2. When creating a PR with forge, use:
;;    - M-g to generate a PR description
;;    - M-r to generate with rationale
;;
;; When forge inserts a PR template into the buffer, gptel-forge-prs will
;; automatically use it as a structure when generating the description.

;;; Code:

(require 'gptel)
(require 'forge)
(require 'forge-post)
(require 'magit-git)

(defgroup gptel-forge-prs nil
  "Generate PR descriptions using gptel."
  :group 'forge
  :group 'gptel)

(defvar gptel-forge-prs-prompt-default
  "You are an expert at writing pull request descriptions. Your job is to write a clear, concise PR description that summarizes the changes.

If a PR template is provided, follow its structure exactly:
- Keep all section headers as-is
- Fill in each section with relevant information based on the code changes
- If a section doesn't apply, write 'N/A' under that section
- Do not leave any section blank

If no template is provided, the PR description should include:
- A brief summary of what the changes do (1-2 sentences)
- Key changes or features added
- Any important implementation details worth noting

Guidelines:
- Be concise and to the point
- Focus on the \"why\" and \"what\", not just the \"how\"
- Use bullet points for listing multiple changes
- Do not include the raw diff output in the description
- Only return the PR description in your response, no meta-commentary

Format (when no template is provided):
- Start with a title line (without a # prefix)
- Follow with a blank line
- Then the body of the description"
  "Default prompt for generating PR descriptions.")

(defvar gptel-forge-prs-prompt-conventional
  "You are an expert at writing pull request descriptions following conventional commit style. Your job is to write a clear, concise PR description.

The PR title should be structured as:
    <type>(<optional scope>): <description>

Types: build, chore, ci, docs, feat, fix, perf, refactor, style, test

If a PR template is provided, follow its structure exactly after the title:
- Keep all section headers as-is
- Fill in each section with relevant information based on the code changes
- If a section doesn't apply, write 'N/A' under that section

If no template is provided, the body should include:
- A brief summary of what the changes do
- Key changes or features (as bullet points if multiple)
- Any breaking changes or important notes

Guidelines:
- Keep the title under 72 characters
- Be concise and focus on the \"why\"
- Do not include raw diff output
- Only return the PR description, no meta-commentary

Format:
- First line is the title (type(scope): description)
- Blank line
- Body with summary and details (or follow the provided template)"
  "Conventional commits style prompt for generating PR descriptions.")

(defcustom gptel-forge-prs-pr-prompt gptel-forge-prs-prompt-default
  "The prompt to use for generating a PR description.
The prompt should consider that the input will be a diff of changes
between the source and target branches."
  :type 'string
  :group 'gptel-forge-prs)

(defcustom gptel-forge-prs-use-buffer-template t
  "Whether to include existing buffer content as a template for the LLM.
When non-nil and the buffer has content, gptel-forge-prs will pass the
existing buffer content (typically a PR template inserted by forge) to
the LLM to use as a structure for the generated description."
  :type 'boolean
  :group 'gptel-forge-prs)

(custom-declare-variable
 'gptel-forge-prs-model nil
 "The gptel model to use, defaults to `gptel-model' if nil.

See `gptel-model' for documentation.

If set to a model that uses a different backend than
`gptel-backend', also requires `gptel-forge-prs-backend' to be
set to the correct backend."
 :type (get 'gptel-model 'custom-type)
 :group 'gptel-forge-prs)

(custom-declare-variable
 'gptel-forge-prs-backend nil
 "The gptel backend to use, defaults to `gptel-backend' if nil.

See `gptel-backend' for documentation."
 :type (get 'gptel-backend 'custom-type)
 :group 'gptel-forge-prs)

(defvar gptel-forge-prs-rationale-buffer "*gptel-forge-prs Rationale*"
  "Buffer name for entering rationale for PR description generation.")

(defvar-local gptel-forge-prs--current-post-buffer nil
  "Buffer where PR description is being generated.")

(defvar-local gptel-forge-prs--source-branch nil
  "Source branch for PR generation.")

(defvar-local gptel-forge-prs--target-branch nil
  "Target branch for PR generation.")

(defun gptel-forge-prs--request (&rest args)
  "Call `gptel-request' with ARGS.
Respects configured model/backend options."
  (declare (indent 1))
  (let* ((gptel-backend (or gptel-forge-prs-backend gptel-backend))
         (gptel-model (or gptel-forge-prs-model gptel-model)))
    (apply #'gptel-request args)))

(defun gptel-forge-prs--get-buffer-template ()
  "Get the existing buffer content to use as a template.
Returns the buffer content excluding the forge comment header,
or nil if the buffer is empty or `gptel-forge-prs-use-buffer-template' is nil."
  (when gptel-forge-prs-use-buffer-template
    (let ((content (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
      ;; Remove the forge comment header (lines starting with "# ")
      (when (and content (not (string-empty-p content)))
        (with-temp-buffer
          (insert content)
          (goto-char (point-min))
          ;; Skip lines starting with "# "
          (while (and (not (eobp))
                      (looking-at "^# "))
            (forward-line 1))
          (let ((result (string-trim (buffer-substring-no-properties (point) (point-max)))))
            (unless (string-empty-p result)
              result)))))))

(defun gptel-forge-prs--get-diff (source target)
  "Get the diff between SOURCE and TARGET branches for PR."
  (when (and source target)
    (let ((diff (magit-git-output "diff" (format "%s...%s" target source))))
      (if (or (null diff) (string-empty-p diff))
          (error "No diff found between %s and %s" target source)
        diff))))

(defun gptel-forge-prs--generate (source target callback &optional rationale buffer-template)
  "Generate a PR description for SOURCE to TARGET branches.
Invokes CALLBACK with the generated description when done.
Optional RATIONALE provides context for why the changes were made.
BUFFER-TEMPLATE is existing buffer content to use as a template structure."
  (let* ((diff (gptel-forge-prs--get-diff source target))
         (prompt (concat
                  ;; Add rationale if provided
                  (when (and rationale (not (string-empty-p rationale)))
                    (format "Why these changes were made: %s\n\n" rationale))
                  ;; Add buffer template if provided
                  (when (and buffer-template (not (string-empty-p buffer-template)))
                    (format "PR template to follow:\n%s\n\n" buffer-template))
                  ;; Add the diff
                  (format "Code changes:\n%s" diff))))
    (gptel-forge-prs--request prompt
      :system gptel-forge-prs-pr-prompt
      :context nil
      :callback (lambda (response info)
                  (cond
                   ((stringp response)
                    (funcall callback response))
                   ((and (consp response) (eq (car response) 'reasoning))
                    nil) ; silently ignore reasoning traces
                   ((plist-get info :error)
                    (message "gptel-forge-prs: Error: %s"
                             (or (plist-get (plist-get info :error) :message)
                                 (plist-get info :error))))
                   (t
                    (message "gptel-forge-prs: Failed to generate description (status: %s)"
                             (plist-get info :status))))))))

(defun gptel-forge-prs-generate-description ()
  "Generate a PR description when in the forge post buffer.
This command is available when editing a new pull-request."
  (interactive)
  (unless (and (derived-mode-p 'forge-post-mode)
               (eq forge-edit-post-action 'new-pullreq))
    (user-error "Not in a new pull-request buffer"))
  (let ((source forge--buffer-head-branch)
        (target forge--buffer-base-branch)
        (buf (current-buffer))
        (template (gptel-forge-prs--get-buffer-template)))
    (unless (and source target)
      (user-error "Source or target branch not set"))
    (message "gptel-forge-prs: Generating PR description...")
    (gptel-forge-prs--generate
     source target
     (lambda (description)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (erase-buffer)
           (insert "# ")
           (save-excursion
             (insert description)))))
     nil
     template)))

(define-derived-mode gptel-forge-prs-rationale-mode text-mode "gptel-forge-prs-Rationale"
  "Mode for entering PR rationale before generating description."
  (local-set-key (kbd "C-c C-c") #'gptel-forge-prs--submit-rationale)
  (local-set-key (kbd "C-c C-k") #'gptel-forge-prs--cancel-rationale))

(defun gptel-forge-prs--setup-rationale-buffer ()
  "Setup the rationale buffer with proper guidance."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert ";;; WHY are you making these changes? (optional)\n")
    (insert ";;; Press C-c C-c to generate PR description, C-c C-k to cancel\n")
    (insert ";;; Leave empty to generate without rationale\n")
    (insert ";;; ────────────────────────────────────────────────────────\n")
    (add-text-properties (point-min) (point)
                         '(face font-lock-comment-face read-only t))
    (insert "\n")
    (goto-char (point-max))))

(defun gptel-forge-prs--submit-rationale ()
  "Submit rationale and proceed with PR description generation."
  (interactive)
  (let ((rationale (string-trim
                    (buffer-substring-no-properties
                     (save-excursion
                       (goto-char (point-min))
                       (while (and (not (eobp))
                                   (get-text-property (point) 'read-only))
                         (forward-char))
                       (point))
                     (point-max))))
        (source gptel-forge-prs--source-branch)
        (target gptel-forge-prs--target-branch)
        (buf gptel-forge-prs--current-post-buffer)
        (template (with-current-buffer gptel-forge-prs--current-post-buffer
                    (gptel-forge-prs--get-buffer-template))))
    (quit-window t)
    (message "gptel-forge-prs: Generating PR description with rationale...")
    (gptel-forge-prs--generate
     source target
     (lambda (description)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (erase-buffer)
           (insert "# ")
           (save-excursion
             (insert description)))))
     rationale
     template)))

(defun gptel-forge-prs--cancel-rationale ()
  "Cancel rationale input and abort PR description generation."
  (interactive)
  (quit-window t)
  (message "PR description generation canceled."))

(defun gptel-forge-prs-generate-description-with-rationale ()
  "Generate a PR description with rationale when in the forge post buffer.
This opens a buffer to enter context about why the changes were made,
which helps the LLM generate a better description."
  (interactive)
  (unless (and (derived-mode-p 'forge-post-mode)
               (eq forge-edit-post-action 'new-pullreq))
    (user-error "Not in a new pull-request buffer"))
  (let ((post-buffer (current-buffer))
        (source forge--buffer-head-branch)
        (target forge--buffer-base-branch))
    (unless (and source target)
      (user-error "Source or target branch not set"))
    (let ((buffer (get-buffer-create gptel-forge-prs-rationale-buffer)))
      (with-current-buffer buffer
        (gptel-forge-prs-rationale-mode)
        (gptel-forge-prs--setup-rationale-buffer)
        (setq gptel-forge-prs--current-post-buffer post-buffer)
        (setq gptel-forge-prs--source-branch source)
        (setq gptel-forge-prs--target-branch target))
      (pop-to-buffer buffer))))

;;;###autoload
(defun gptel-forge-prs-install ()
  "Install gptel-forge-prs functionality.
Adds keybindings to `forge-post-mode-map' for generating PR descriptions."
  (require 'forge-post)
  (define-key forge-post-mode-map (kbd "M-g") #'gptel-forge-prs-generate-description)
  (define-key forge-post-mode-map (kbd "M-r") #'gptel-forge-prs-generate-description-with-rationale))

;;;###autoload
(defun gptel-forge-prs-uninstall ()
  "Uninstall gptel-forge-prs functionality."
  (require 'forge-post)
  (define-key forge-post-mode-map (kbd "M-g") nil)
  (define-key forge-post-mode-map (kbd "M-r") nil))

(provide 'gptel-forge-prs)
;;; gptel-forge-prs.el ends here
