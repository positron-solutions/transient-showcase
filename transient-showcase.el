;;; transient-showcase.el --- transient features & behavior showcase -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Positron Solutions

;; Author: Positron <contact@positron.solutions>
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Homepage: http://github.com/positron-solutions/transient-showcase

;;; License notice:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is created from the README and serves as a fast way to load
;; all of the examples without tangling the org document.  This is appropriate
;; if you just want to quickly browse through the examples and see their
;; source code.
;;
;; M-x tsc-showcase contains most of the prefixes and can be bound for
;; use as a quick reference.  Just use transient's help for each
;; command to see the source.  C-h <suffix key>.
;;

;;; Code:

(require 'transient)
(require 'org-id)


(defun tsc-suffix-wave ()
  "Wave at the user."
  (interactive)
  (message "Waves at the user at: %s." (current-time-string)))


(defvar tsc-busy nil "Are we busy?")

(defun tsc--busy-p () "Are we busy?" tsc-busy)

(transient-define-suffix tsc--toggle-busy ()
  "Toggle busy."
  (interactive)
  (setf tsc-busy (not tsc-busy))
  (message (propertize (format "busy: %s" tsc-busy)
                       'face 'success)))


(transient-define-suffix tsc-suffix-show-level ()
  "Show the current transient's level."
  :transient t
  (interactive)
  (message "Current level: %s" (oref transient-current-prefix level)))


;; Because command names are used to store and lookup child levels, we have
;; define a macro to generate unqiquely named wavers.  See #153 at
;; https://github.com/magit/transient/issues/153
(defmacro tsc--define-waver (name)
  "Define a new suffix with NAME tsc--wave-NAME."
  `(transient-define-suffix ,(intern (format "tsc--wave-%s" name)) ()
     ,(format "Wave at the user %s" name)
     :transient t
     (interactive)
     (message (format "Waves %s at %s" ,name (current-time-string)))))

;; Each form results in a unique suffix definition.
(tsc--define-waver "surely")
(tsc--define-waver "normally")
(tsc--define-waver "non-essentially")
(tsc--define-waver "definitely")
(tsc--define-waver "eventually")
(tsc--define-waver "hidden")
(tsc--define-waver "inquisitively")
(tsc--define-waver "writingly")
(tsc--define-waver "switchedly")
(tsc--define-waver "unswitchedly")


(transient-define-suffix tsc-suffix-print-args (the-prefix-arg)
  "Report the PREFIX-ARG, prefix's scope, and infix values."
  :transient 'transient--do-call
  (interactive "P")
  (let ((args (transient-args (oref transient-current-prefix command)))
        (scope (transient-scope)))
    (message "prefix-arg: %S \nprefix's scope value: %S \ntransient-args: %S"
             the-prefix-arg scope args)))

;; tsc-suffix-print-args command is incidentally created

(transient-define-prefix tsc-hello ()
  "Prefix that is minimal and uses an anonymous command suffix."
  [("s" "call suffix"
    (lambda ()
      (interactive)
      (message "Called a suffix")))])

;; First, use M-x org-babel-execute-src-blk to cause `tsc-hello' to be
;; defined
;; Second, M-x `eval-last-sexp' with your point at the end of the line below
;; (tsc-hello)

(transient-define-suffix tsc-suffix-wave-macroed ()
  "Prefix that waves with macro-defined suffix."
  :transient t
  :key "T"
  :description "wave from macro definition"
  (interactive)
  (message "Waves from a macro definition at: %s" (current-time-string)))

;; Suffix definition creates a command
;; (tsc-suffix-wave-macroed)
;; Because that's where the suffix object is stored
;; (get 'tsc-suffix-wave-macroed 'transient--suffix)

;; tsc-suffix-wave-suffix defined above
(transient-define-prefix tsc-wave-macro-defined ()
  "Prefix to wave using a macro-defined suffix."
  [(tsc-suffix-wave-macroed)])
;; note, information moved from prefix to the suffix.

;; (tsc-wave-macro-defined)

(defun tsc--wave-override ()
  "Vanilla command used to override suffix's commands."
  (interactive)
  (message "This suffix was overridden.  I am what remains."))

(transient-define-prefix tsc-wave-overridden ()
  "Prefix that waves with overridden suffix behavior."
  [(tsc-suffix-wave-macroed
    :transient nil
    :key "O"
    :description "wave overridingly"
    :command tsc--wave-override)]) ; we overrode what the suffix even does

;; (tsc-wave-overridden)

(transient-define-prefix tsc-layout-descriptions ()
  "Prefix with descriptions specified with slots."
  ["Let's Give This Transient a Title\n" ; yes the newline works
   ["Group One"
    ("wo" "wave once" tsc-suffix-wave)
    ("wa" "wave again" tsc-suffix-wave)]

   ["Group Two"
    ("ws" "wave some" tsc-suffix-wave)
    ("wb" "wave better" tsc-suffix-wave)]]

  ["Bad title" :description "Group of Groups"
   ["Group Three"
    ("k" "bad desc" tsc-suffix-wave :description "key-value wins")
    ("n" tsc-suffix-wave :description "no desc necessary")]
   [:description "Key Only Def"
                 ("wt" "wave too much" tsc-suffix-wave)
                 ("we" "wave excessively" tsc-suffix-wave)]])

;; (tsc-layout-descriptions)

(transient-define-prefix tsc-layout-dynamic-descriptions ()
  "Prefix that generate descriptions dynamically when transient is shown."
  ;; group using function-name to generate description
  [:description current-time-string
                ("-s" "--switch" "switch=") ; switch just to cause updates
                ;; single suffix with dynamic description
                ("wa" tsc-suffix-wave
                 :description (lambda ()
                                (format "Wave at %s" (current-time-string))))]
  ;; group with anonymoous function generating description
  [:description (lambda ()
                  (format "Group %s" (org-id-new)))
                ("wu" "wave uniquely" tsc-suffix-wave)])

;; (tsc-layout-dynamic-descriptions)

(defun tsc--random-info ()
  (format "Temperature outside: %d" (random 100)))

(transient-define-prefix tsc-information ()
  "Prefix that displays some information."
  ["Group Header"
   (:info "Basic info")
   (:info #'tsc--random-info)
   (:info "Use :format to remove whitespace" :format "%d")
   ("k" :info "Keys will be greyed out")
   "" ; empty line
   ("wg" "wave greenishly" tsc-suffix-wave)])

(transient-define-prefix tsc-layout-stacked ()
  "Prefix with layout that stacks groups on top of each other."
  ["Top Group" ("wt" "wave top" tsc-suffix-wave)]
  ["Bottom Group" ("wb" "wave bottom" tsc-suffix-wave)])

;; (tsc-layout-stacked)

(transient-define-prefix tsc-layout-columns ()
  "Prefix with side-by-side layout."
  [["Left Group" ("wl" "wave left" tsc-suffix-wave)]
   ["Right Group" ("wr" "wave right" tsc-suffix-wave)]])

;; (tsc-layout-columns)

(transient-define-prefix tsc-layout-stacked-columns ()
  "Prefix with stacked columns layout."
  ["Top Group"
   ("wt" "wave top" tsc-suffix-wave)]

  [["Left Group"
    ("wl" "wave left" tsc-suffix-wave)]
   ["Right Group"
    ("wr" "wave right" tsc-suffix-wave)]])

;; (tsc-layout-stacked-columns)

(transient-define-prefix tsc-layout-spaced-out ()
  "Prefix lots of spacing for users to space out at."
  ["" ; cannot add another empty string because it will mix suffixes with groups
   ["Left Group"
    ""
    ("wl" "wave left" tsc-suffix-wave)
    ("L" "wave lefter" tsc-suffix-wave)
    ""
    ("bl" "wave bottom-left" tsc-suffix-wave)
    ("z" "zone\n" zone)] ; the newline does pad

   [[]] ; empty vector will do nothing

   [""] ; vector with just empty line has no effect

   ;; empty group will be ignored
   ;; (useful for hiding in dynamic layouts)
   ["Empty Group\n"]

   ["Right Group"
    ""
    ("wr" "wave right" tsc-suffix-wave)
    ("R" "wave righter" tsc-suffix-wave)
    ""
    ("br" "wave bottom-right" tsc-suffix-wave)]])

;; (tsc-layout-spaced-out)

(transient-define-prefix tsc-layout-the-grid ()
  "Prefix with groups in a grid-like arrangement."

  [:description
   "The Grid\n" ; must use slot or macro is confused
   ["Left Column" ; note, no newline
    ("ltt" "left top top" tsc-suffix-wave)
    ("ltb" "left top bottom" tsc-suffix-wave)
    ""
    ("lbt" "left bottom top" tsc-suffix-wave)
    ("lbb" "left bottom bottom" tsc-suffix-wave)] ; note, no newline

   ["Right Column\n"
    ("rtt" "right top top" tsc-suffix-wave)
    ("rtb" "right top bottom" tsc-suffix-wave)
    ""
    ("rbt" "right bottom top" tsc-suffix-wave)
    ("rbb" "right bottom bottom\n" tsc-suffix-wave)]])

;; (tsc-layout-the-grid)

(transient-define-prefix tsc-layout-explicit-classes ()
  "Prefix with group class used to explicitly specify layout."
  [ :class transient-row "Row"
    ("l" "wave left" tsc-suffix-wave)
    ("r" "wave right" tsc-suffix-wave)]
  [ :class transient-column "Column"
    ("t" "wave top" tsc-suffix-wave)
    ("b" "wave bottom" tsc-suffix-wave)])

;; (tsc-layout-explicit-classes)

(transient-define-prefix tsc-stay-transient ()
  "Prefix where some suffixes do not exit."
  ["Exit or Not?"

   ;; this suffix will not exit after calling sub-prefix
   ("we" "wave & exit" tsc-wave-overridden)
   ("ws" "wave & stay" tsc-suffix-wave :transient t)])

;; (tsc-stay-transient)

(transient-define-prefix tsc--simple-child ()
  ["Simple Child"
   ("wc" "wave childishly" tsc-suffix-wave)])

(transient-define-prefix tsc-simple-parent ()
  "Prefix that calls a child prefix."
  ["Simple Parent"
   ("w" "wave parentally" tsc-suffix-wave)
   ("b" "become child" tsc--simple-child)])

;; (tsc--simple-child)
;; (tsc-simple-parent)

(transient-define-prefix tsc-simple-parent-with-return ()
  "Prefix with a child prefix that returns."
  ["Parent With Return"
   ("w" "wave parentally" tsc-suffix-wave)
   ("b" "become child with return" tsc--simple-child :transient t)])

;; Child does not "return" when called independently
;; (tsc--simple-child)
;; (tsc-simple-parent-with-return)

(transient-define-suffix tsc-suffix-setup-child ()
  "A suffix that uses `transient-setup' to manually load another transient."
  (interactive)
  ;; note that it's usually during the post-command side of calling the
  ;; command that the actual work to set up the transient will occur.
  ;; This is an implementation detail because it depends if we are calling
  ;; `transient-setup' while already transient or not.
  (transient-setup 'tsc--simple-child))

(transient-define-prefix tsc-parent-with-setup-suffix ()
  "Prefix with a suffix that calls `transient-setup'."
  ["Simple Parent"
   ("wp" "wave parentally" tsc-suffix-wave :transient t) ; remain transient

   ;; You may need to specify a different pre-command (the :transient) key
   ;; because we need to clean up this transient or create some conditions
   ;; to trigger the following transient correctly.  This example will
   ;; work with `transient--do-replace' or no custom pre-command

   ("bc" "become child" tsc-suffix-setup-child
    :transient transient--do-replace)])

;; (tsc-parent-with-setup-suffix)

(defvar tsc--complex nil "Show complex menu or not.")

(transient-define-suffix tsc--toggle-complex ()
  "Toggle `tsc--complex'."
  :transient t
  :description (lambda () (format "toggle complex: %s" tsc--complex))
  (interactive)
  (setf tsc--complex (not tsc--complex))
  (message (propertize (concat "Complexity set to: "
                               (if tsc--complex "true" "false"))
                       'face 'success)))

(transient-define-prefix tsc-complex-messager ()
  "Prefix that sends complex messages, unles `tsc--complex' is nil."
  ["Send Complex Messages"
   ("s" "snow people"
    (lambda () (interactive)
      (message (propertize "snow people! ☃" 'face 'success))))
   ("k" "kitty cats"
    (lambda () (interactive)
      (message (propertize "🐈 kitty cats! 🐈" 'face 'success))))
   ("r" "radiations"
    (lambda () (interactive)
      (message (propertize "Oh no! radiation! ☢" 'face 'error)))
    ;; radiation is dangerous!
    :transient transient--do-exit)]

  (interactive)
  ;; The command body either sets up the transient or simply returns
  ;; This is the "early return" we're talking about.
  (if tsc--complex
      (transient-setup 'tsc-complex-messager)
    (message "Simple and boring!")))

(transient-define-prefix tsc-simple-messager ()
  "Prefix that toggles child behavior!"
  [["Send Message"
    ;; using `transient--do-recurse' causes suffixes in tsc-child to perform
    ;; `transient--do-return' so that we come back to this transient.
    ("m" "message" tsc-complex-messager :transient transient--do-recurse)]
   ["Toggle Complexity"
    ("t" tsc--toggle-complex)]])

;; (tsc-simple-messager) ; toggle complexity on

;; Because `tsc--complex' is in a defvar, its behavior persists when called
;; independently.  Because `tsc-simple-messager' is not in the menu stack when
;; called this way, no return will be performed.
;; (tsc-complex-messager)

(transient-define-suffix tsc--suffix-interactive-buffer-name (buffer-name)
  "An interactive suffix that obtains a buffer name from the user.
This uses the short interactive code."
  (interactive "b")
  (message "You selected: %s" buffer-name))

(transient-define-suffix tsc--suffix-interactive-string (user-input)
  "An interactive suffix that evalutates its arguments exlicitly."
  (interactive (list
                (read-string "Please just tell me what you want!: ")))
  (message "I think you want: %s" user-input))

(transient-define-prefix tsc-interactive ()
  "Prefix with interactive suffixes."
  ["Interactive Command Suffixes"
   ("s" "enter string" tsc--suffix-interactive-string)
   ("b" "select buffer" tsc--suffix-interactive-buffer-name)
   ;; using a normal command with a user query in its interactive form
   ("f" "find file" find-file)])

;; (tsc-interactive)

(defvar tsc-creativity-subjective "I Just press buttons on my gen-AI"
  "An unverifiable statement about the user's creativity.")

(defvar tsc-creativity-objective 30
  "User's creativity percentile as assesed by our oracle")

(defun tsc-creativity-subjective-update (creativity)
  "Update the users creativity assessment subjectively."
  (interactive (list (read-string "User subjective creativity: "
                                  tsc-creativity-subjective)))
  (setq tsc-creativity-subjective creativity)
  (message "Subjective creativity updated: %s" tsc-creativity-subjective))

(defun tsc-creativity-objective-update (creativity)
  "Update the users creativity assessment objectively."
  (interactive (list (read-number "User objective creativity: "
                            tsc-creativity-objective)))
  (if (and (integerp creativity) (>= 100 creativity -1))
      (progn (setq tsc-creativity-objective creativity)
             (message "Objective creativity updated: %d"
                      tsc-creativity-objective))
    (user-error "Only integers between 0 and 100 allowed")))

(defun tsc--creativity-subjective-describe ()
  "Describe command and display current subjective state."
  (format "subjective: %s" (propertize tsc-creativity-subjective
                                       'face 'transient-value)))

(defun tsc--creativity-objective-describe ()
  "Describe command and display current objective state."
  (format "objective: %s"
          (propertize (number-to-string tsc-creativity-objective)
                      'face 'transient-value)))

;; When we can't jsut use a symbol for what we want to display, write a function
(defun tsc--creativity-display ()
  "Returns a formatted assessment of the users value as a human being."
  (format "User creativity score of %s self-assesses: %s"
          (propertize tsc-creativity-subjective 'face 'transient-value)
          (propertize (number-to-string tsc-creativity-objective)
                      'face 'transient-value)))

(transient-define-prefix tsc-defvar-settings ()
  "A prefix demonstrating file-based ad-hoc persistence."
  ;; Note the sharpquote (#') used to distinguish a symbol from just a function
  ["Creativity\n"
   (:info #'tsc--creativity-display :format " %d")
   " "
   ("d" tsc-creativity-subjective-update :transient t
    :description tsc--creativity-subjective-describe)
   ("o" tsc-creativity-objective-update :transient t
    :description tsc--creativity-objective-describe)])

;; (tsc-defvar-settings)

(defvar-local tsc--mode-line-memento nil
  "A value we can use to restore the `mode-line-format'.")

(defun tsc--toggle-mode-line ()
  "Save and restore the mode line like a pro."
  (interactive)
  (if (null tsc--mode-line-memento)
      (setq tsc--mode-line-memento
            (buffer-local-set-state mode-line-format
                                    "Wh000pTY D000PTY D0000!"))
    (buffer-local-restore-state tsc--mode-line-memento)
    (setq tsc--mode-line-memento nil))
  ;; The mode line won't always redraw if we don't tell the command
  ;; loop about what we did.
  (force-window-update))

(transient-define-prefix tsc-buffer-local ()
  ["Mode Line Gizmo"
   ("m" "toggle modeline" tsc--toggle-mode-line :transient t)])

;; (tsc-buffer-local)

;; This just sets the default group for the following defcustom
(defgroup tsc-creativity nil "Creativity" :group 'local)

;; This defvar is a bit longer than strictly necessary.  Lots of users load
;; no-littering early in their init to make Elisp programs save files in more
;; uniform locations.  This expression respects no-littering or works without
;; it.
(defcustom tsc-creativity-file
  (if (and (featurep 'no-littering) (require 'no-littering nil t))
      (no-littering-expand-var-file-name "tsc-creativity.el")
    (expand-file-name "tsc-creativity.el" user-emacs-directory))
  "Where settings are saved to."
  :type 'file)

(defun tsc-creativity-save ()
  "Save the current creativity states."
  (interactive)
  (with-temp-buffer
    " *tsc-peristence*"
    (pp            ; pretty print
     ;; Like writing a macro, you just use quasi-quoting to stitch
     ;; together the structue you want to be in the output.
     `(setq tsc-creativity-subjective ,tsc-creativity-subjective
            tsc-creativity-objective ,tsc-creativity-objective)
     (current-buffer))
    (write-file tsc-creativity-file)
    (message "Transient showcase setting saved!")))

(defun tsc-creativity-load ()
  "Yes, just load what we wrote."
  (interactive)
  (if (file-exists-p tsc-creativity-file)
      (load tsc-creativity-file)
    (user-error "No saved settings exist")))

(defun tsc-creativity-visit-settings ()
  "Show us what we wrote."
  (interactive)
  (if (file-exists-p tsc-creativity-file)
      (find-file tsc-creativity-file)
    (user-error "No saved settings exist")))

(transient-define-prefix tsc-persistent-settings ()
"A prefix demonstrating file-based ad-hoc persistence."
:refresh-suffixes t
;; Note the sharpquote (#') used to distinguish a symbol from just a function in
;; the :info class.  Info can understand a variable or a function as its value.
["Creativity"
 (:info #'tsc--creativity-display :format " %d")
 ("d" tsc-creativity-subjective-update :transient t
  :description tsc--creativity-subjective-describe)
 ("o" tsc-creativity-objective-update :transient t
  :description tsc--creativity-objective-describe)]
["Persistence"
 ("s" "save" tsc-creativity-save :transient t)
 ("l" "load" tsc-creativity-load :transient t
  :inapt-if-not (lambda () (file-exists-p tsc-creativity-file)))
 ("v" "visit" tsc-creativity-visit-settings :transient t
  :inapt-if-not (lambda () (file-exists-p tsc-creativity-file)))])

;; infix defined with a macro
(transient-define-argument tsc--exclusive-switches ()
  "This is a specialized infix for only selecting one of several values."
  :class 'transient-switches
  :argument-format "--%s-snowcone"
  :argument-regexp "\\(--\\(grape\\|orange\\|cherry\\|lime\\)-snowcone\\)"
  :choices '("grape" "orange" "cherry" "lime"))

(transient-define-prefix tsc-basic-infixes ()
  "Prefix that just shows off many typical infix types."
  ["Infixes"

   ;; from macro
   ("-e" "exclusive switches" tsc--exclusive-switches)

   ;; shorthand definitions
   ("-b" "switch with shortarg" ("-w" "--switch-short"))
   ;; note :short-arg != :key

   ("-s" "switch" "--switch")
   ( "n" "no dash switch" "still works")
   ("-a" "argument" "--argument=" :prompt "Let's argue because: ")

   ;; a bit of inline EIEIO in our shorthand
   ("-n" "never empty" "--non-null=" :always-read t  :allow-empty nil
    :init-value (lambda (obj) (oset obj value "better-than-nothing")))

   ("-c" "choices" "--choice=" :choices (foo bar baz))]

  ["Show Args"
   ("s" "show arguments" tsc-suffix-print-args)])

;; (tsc-basic-infixes)

(defvar tsc--position '(0 0) "A transient prefix location.")

(transient-define-infix tsc--pos-infix ()
  "A location, key, or command symbol."
  :class 'transient-lisp-variable
  :transient t
  :prompt "An expression such as (0 0), \"p\", nil, 'tsc--msg-pos: "
  :variable 'tsc--position)

(transient-define-suffix tsc--msg-pos ()
  "Message the element at location."
  :transient 'transient--do-call
  (interactive)
  ;; lisp variables are not sent in the usual (transient-args) list.
  ;; Just read `tsc--position' directly.
  (let ((suffix (transient-get-suffix
                 transient-current-command tsc--position)))
    (message "%s" (oref suffix description))))

(transient-define-prefix tsc-lisp-variable ()
  "A prefix that updates and uses a lisp variable."
  ["Location Printing"
   [("p" "position" tsc--pos-infix)]
   [("m" "message" tsc--msg-pos)]])

;; (tsc-lisp-variable)

(transient-define-suffix tsc--read-prefix-scope ()
  "Read the scope of the prefix."
  :transient 'transient--do-call
  (interactive)
  (let ((scope (transient-scope)))
    (message "scope: %s" scope)))

(transient-define-suffix tsc--double-scope-re-enter ()
  "Re-enter the current prefix with double the scope."
  ;; :transient 'transient--do-replace ; builds up the stack
  :transient 'transient--do-exit
  (interactive)
  (let ((scope (transient-scope)))
    (if (numberp scope)
        (transient-setup transient-current-command
                         nil nil :scope (* scope 2))
      (message
       (propertize
        (format "scope was non-numeric! %s" scope) 'face 'warning))
      (transient-setup transient-current-command))))

(transient-define-suffix tsc--update-scope-with-prefix-re-enter (new-scope)
  "Re-enter the prefix with double the scope."
  ;; :transient 'transient--do-replace ; builds up the stack
  :transient 'transient--do-exit ; do not build up the stack
  (interactive "P")
  (message "universal arg: %s" new-scope)
  (transient-setup transient-current-command nil nil :scope new-scope))

(transient-define-prefix tsc-scope (scope)
  "Prefix demonstrating use of scope."

  [:description
   (lambda () (format "Scope: %s" (transient-scope)))
   [("r" "read scope" tsc--read-prefix-scope)
    ("d" "double scope" tsc--double-scope-re-enter)
    ("o" "update scope (use prefix argument)"
     tsc--update-scope-with-prefix-re-enter)]]
  (interactive "P")
  (transient-setup 'tsc-scope nil nil :scope scope))

;; Setting an interactive argument for `eval-last-sexp' is a
;; little different
;; (let ((current-prefix-arg 4)) (call-interactively 'tsc-scope))

;; (tsc-scope)
;; Then press "C-u 4 o" to update the scope
;; Then d to double
;; Then r to read
;; ... and so on
;; C-g to exit

(transient-define-suffix tsc-suffix-eat-snowcone (args)
  "Eat the snowcone!
This command can be called from it's parent, `tsc-snowcone-eater' or independently."
  :transient t
  ;; you can use the interactive form of a command to obtain a default value
  ;; from the user etc if the one obtained from the parent is invalid.
  (interactive (list (transient-args 'tsc-snowcone-eater)))

  ;; `transient-arg-value' can (with varying success) pick out individual
  ;; values from the results of `transient-args'.

  (let ((topping (transient-arg-value "--topping=" args))
        (flavor (transient-arg-value "--flavor=" args)))
    (message "I ate a %s flavored snowcone with %s on top!" flavor topping)))

(transient-define-prefix tsc-snowcone-eater ()
  "Prefix demonstrating set & save infix persistence."

  ;; This prefix has a default value that tsc-suffix-eat-snowcone can see
  ;; even before the prefix has been called.
  :value '("--topping=fruit" "--flavor=cherry")

  ;; always-read is used below so that you don't save nil values to history
  ["Arguments"
   ("-t" "topping" "--topping="
    :choices ("ice cream" "fruit" "whipped cream" "mochi")
    :always-read t)
   ("-f" "flavor" "--flavor="
    :choices ("grape" "orange" "cherry" "lime")
    :always-read t)]

  ;; Definitely check out the =C-x= menu
  ["C-x Menu Behaviors"
   ("S" "save snowcone settings"
    (lambda () (interactive) (message "saved!") (transient-save))
    :transient t)
   ("R" "reset snowcone settings"
    (lambda () (interactive) (message "reset!") (transient-reset))
    :transient t)]

  ["Actions"
   ("m" "message arguments" tsc-suffix-print-args)
   ("e" "eat snowcone" tsc-suffix-eat-snowcone)])

;; First call will use the transient's default value
;; M-x tsc-suffix-eat-snowcone or `eval-last-sexp' below
;; (call-interactively 'tsc-suffix-eat-snowcone)
;; (tsc-snowcone-eater)
;; Eat some snowcones with different flavors
;; ...
;; ...
;; ...
;; Now save the value and exit the transient.
;; When you call the suffix independently, it can still read the saved values!
;; M-x tsc-suffix-eat-snowcone or `eval-last-sexp' below
;; (call-interactively 'tsc-suffix-eat-snowcone)

(transient-define-prefix tsc-ping ()
  "Prefix demonstrating history sharing."

  :history-key 'highly-unique-name

  ["Ping"
   ("-g" "game" "--game=")
   ("p" "ping the pong" tsc-pong)
   ("a" "print args" tsc-suffix-print-args :transient nil)])

(transient-define-prefix tsc-pong ()
  "Prefix demonstrating history sharing."

  :history-key 'highly-unique-name

  ["Pong"
   ("-g" "game" "--game=")
   ("p" "pong the ping" tsc-ping)
   ("a" "print args" tsc-suffix-print-args :transient nil)])

;; (tsc-ping)
;; Okay here's where it gets weird
;; 1.  Set the value of game to something and remember it
;; 2.  Press a to print the args
;; 3.  Re-open tsc-ping.
;; 4.  C-x p to load the previous history, see the old value?
;; 5.  p to switch to the tsc-pong transient
;; 6.  C-x p to load the previous history, see the old value from tsc-ping???
;; 7. Note that tsc-pong uses the same history as tsc-ping!

(transient-define-prefix tsc-goldfish ()
  "A prefix that cannot remember anything."
  ["Goldfish"
   ("-r" "rememeber" "--i-remember="
    :unsavable t ; infix isn't saved
    :always-read t ; infix always asks for new value
    ;; overriding the method to provide a starting value
    :init-value (lambda (obj) (oset obj value "nothing")))
   ("a" "print args" tsc-suffix-print-args :transient nil)])

;; (tsc-goldfish)

(transient-define-suffix tsc-suffix-remember-and-wave ()
  "Wave, and force the prefix to set it's saveable infix values."
  (interactive)

  ;; (transient-reset) ; forget
  (transient-set) ; save for this session
  ;; If you combine reset save with reset, you get a reset for future
  ;; sessions only.
  ;; (transient-save) ; save for this and future sessions
  ;; (transient-reset-value some-other-prefix-object)

  (message "Waves at user at: %s.  You will never be forgotten." (current-time-string)))

(transient-define-prefix tsc-elephant ()
  "A prefix that always remembers its infixes."
  ["Elephant"
   ("-r" "rememeber" "--i-remember="
    :always-read t)
   ("w" "remember and wave" tsc-suffix-remember-and-wave)
   ("a" "print args (skips remembering)" tsc-suffix-print-args
    :transient nil)])

;; (tsc-elephant)

(transient-define-prefix tsc-default-values ()
  "A prefix with a default value."

  :value '("--toggle" "--value=5")

  ["Arguments"
   ("t" "toggle" "--toggle")
   ("v" "value" "--value=" :prompt "an integer: ")]

  ["Show Args"
   ("s" "show arguments" tsc-suffix-print-args)])

;; (tsc-default-values)

(transient-define-prefix tsc-enforcing-inputs ()
  "A prefix with enforced input type."

  ["Arguments"
   ("v" "value" "--value=" :prompt "an integer: " :reader transient-read-number-N+)]

  ["Show Args"
   ("s" "show arguments" tsc-suffix-print-args)])

;; (tsc-enforcing-inputs)

(transient-define-prefix tsc-switches-and-arguments (arg)
  "A prefix with switch and argument examples."
  [["Arguments"
    ("-s" "switch" "--switch")
    ("-a" "argument" "--argument=")
    ("t" "toggle" "--toggle")
    ("v" "value" "--value=")]

   ["More Arguments"
    ("-f" "argument with forced class" "--forced-class "
     :class transient-option)
    ("I" "argument with inline" ("-i" "--inline-shortarg="))
    ("S" "inline shortarg switch" ("-n" "--inline-shortarg-switch"))]]

  ["Commands"
   ("w" "wave some" tsc-suffix-wave)
   ("s" "show arguments" tsc-suffix-print-args)])
;; use to `tsc-suffix-print-args' to analyze the switch values

;; (tsc-switches-and-arguments)

(transient-define-infix tsc--random-init-infix ()
  "Switch on and off."
  :argument "--switch"
  :shortarg "-s" ; will be used for :key when key is not set
  :description "switch"
  :init-value (lambda (obj)
                (oset obj value
                      (eq 0 (random 2))))) ; write t with 50% probability

(transient-define-prefix tsc-maybe-on ()
  "A prefix with a randomly intializing switch."
  ["Arguments"
   (tsc--random-init-infix)]
  ["Show Args"
   ("s" "show arguments" tsc-suffix-print-args)])

;; (tsc-maybe-on)
;; (tsc-maybe-on)
;; ...
;; Run the command a few times to see the random initialization of
;; `tsc--random-init-infix'
;; It will only take more than ten tries for one in a thousand users.
;; Good luck.

(transient-define-argument tsc--animals-argument ()
  "Animal picker."
  :argument "--animals="
  ;; :multi-value t
  ;; :multi-value t means multiple options can be selected at once, such as:
  ;; --animals=fox,otter,kitten etc
  :class 'transient-option
  :choices '("fox" "kitten" "peregrine" "otter"))

(transient-define-prefix tsc-animal-choices ()
  "Prefix demonstrating selecting animals from choices."
  ["Arguments"
   ("-a" "--animals=" tsc--animals-argument)]
  ["Show Args"
   ("s" "show arguments" tsc-suffix-print-args)])

;; (tsc-animal-choices)

(transient-define-argument tsc--snowcone-flavor ()
  :description "Flavor of snowcone."
  :class 'transient-switches
  :key "f"
  :argument-format "--%s-snowcone"
  :argument-regexp "\\(--\\(grape\\|orange\\|cherry\\|lime\\)-snowcone\\)"
  :choices '("grape" "orange" "cherry" "lime"))

(transient-define-prefix tsc-exclusive-switches ()
  "Prefix demonstrating exclusive switches."
  :value '("--orange-snowcone")

  ["Arguments"
   (tsc--snowcone-flavor)]
  ["Show Args"
   ("s" "show arguments" tsc-suffix-print-args)])

;; (tsc-exclusive-switches)

(transient-define-prefix tsc-incompatible ()
  "Prefix demonstrating incompatible switches."
  ;; update your transient version if you experience #129 / #155
  :incompatible '(("--switch" "--value=")
                  ("--switch" "--toggle" "--flip")
                  ("--argument=" "--value=" "--special-arg="))

  ["Arguments"
   ("-s" "switch" "--switch")
   ("-t" "toggle" "--toggle")
   ("-f" "flip" "--flip")

   ("-a" "argument" "--argument=")
   ("v" "value" "--value=")
   ("C-a" "special arg" "--special-arg=")]

  ["Show Args"
   ("s" "show arguments" tsc-suffix-print-args)])

;; (tsc-incompatible)

(defun tsc--animal-choices (_complete-me _predicate flag)
  "Programmed completion for animal choice.
_COMPLETE-ME: whatever the user has typed so far
_PREDICATE: function you should use to filter candidates (only nil seen so far)
FLAG: request for metadata (which can be disrespected)"

  ;; if you want to respect metadata requests, here's what the form might
  ;; look like, but no behavior was observed.
  (if (eq flag 'metadata)
      '(metadata . '((annotation-function . (lambda (c) "an annotation"))))

    ;; when not handling a metadata request from completions, use some
    ;; logic to generate the choices, possibly based on input or some time
    ;; / context sensitive process.  FLAG will be `t' when these are
    ;; reqeusted.
    (if (eq 0 (random 2))
        '("fox" "kitten" "otter")
      '("ant" "peregrine" "zebra"))))

(transient-define-prefix tsc-choices-with-completions ()
  "Prefix with completions for choices."
  ["Arguments"
   ("-a" "Animal" "--animal="
    :always-read t ; don't allow unsetting, just read a new value
    :choices tsc--animal-choices)]
  ["Show Args"
   ("s" "show arguments" tsc-suffix-print-args)])

;; (tsc-choices-with-completions)

(defun tsc--quit-cowsay ()
  "Kill the cowsay buffer and exit."
  (interactive)
  (kill-buffer "*cowsay*"))

(defun tsc--cowsay-buffer-exists-p ()
  "Visibility predicate."
  (not (equal (get-buffer "*cowsay*") nil)))

(transient-define-suffix tsc--cowsay-clear-buffer (&optional buffer)
  "Delete the *cowsay* buffer.  Optional BUFFER name."
  :transient 'transient--do-call
  :if 'tsc--cowsay-buffer-exists-p
  (interactive) ; todo look at "b" interactive code

  (save-excursion
    (let ((buffer (or buffer "*cowsay*")))
      (set-buffer buffer)
      (delete-region 1 (+ 1 (buffer-size))))))

(transient-define-suffix tsc--cowsay (&optional args)
  "Run cowsay."
  (interactive (list (transient-args transient-current-command)))
  (let* ((buffer "*cowsay*")
         ;; TODO ugly
         (cowmsg (if args (transient-arg-value "--message=" args) nil))
         (cowmsg (if cowmsg (list cowmsg) nil))
         (args (if args
                   (seq-filter
                    (lambda (s) (not (string-prefix-p "--message=" s))) args)
                 nil))
         (args (if args
                   (if cowmsg
                       (append args cowmsg)
                     args)
                 cowmsg)))

    (when (tsc--cowsay-buffer-exists-p)
      (tsc--cowsay-clear-buffer))
    (apply #'call-process "cowsay" nil buffer nil args)
    (switch-to-buffer buffer)))

(transient-define-prefix tsc-cowsay ()
  "Say things with animals!"
  ;; only one kind of eyes is meaningful at a time
  :incompatible '(("-b" "-g" "-p" "-s" "-t" "-w" "-y"))

  ["Message"
   ("m" "message" "--message=" :always-read t)]
  ;; always-read, so clear by entering empty string
  [["Built-in Eyes"
    ("b" "borg" "-b")
    ("g" "greedy" "-g")
    ("p" "paranoid" "-p")
    ("s" "stoned" "-s")
    ("t" "tired" "-t")
    ("w" "wired" "-w")
    ("y" "youthful" "-y")]
   ["Actions"
    ("c" "cowsay" tsc--cowsay :transient transient--do-call)
    ""
    ("d" "delete buffer" tsc--cowsay-clear-buffer)
    ("q" "quit" tsc--quit-cowsay)]])

;; (tsc-cowsay)

(transient-define-prefix tsc-visibility-predicates ()
  "Prefix with visibility predicates.
Try opening this prefix in buffers with modes deriving from different
abstract major modes."
  ["Empty Groups Not Displayed"
   ;; in org mode for example, this group doesn't appear.
   ("we" "wave elisp" tsc-suffix-wave :if-mode emacs-lisp-mode)
   ("wc" "wave in C" tsc-suffix-wave :if-mode cc-mode)]

  ["Lists of Modes"
   ("wm" "wave multiply" tsc-suffix-wave :if-mode (dired-mode gnus-mode))]

  [["Function Predicates"
    ;; note, after toggling, the transient needs to be re-displayed for the
    ;; predicate to take effect
    ("tb" "toggle busy" tsc--toggle-busy :transient t)
    ("bw" "wave busily" tsc-suffix-wave :if tsc--busy-p)]

   ["Programming Actions"
    :if-derived prog-mode
    ("pw" "wave programishly" tsc-suffix-wave)
    ("pe" "wave in elisp" tsc-suffix-wave :if emacs-lisp-mode)]
   ["Special Mode Actions"
    :if-derived special-mode
    ("sw" "wave specially" tsc-suffix-wave)
    ("sd" "wave dired" tsc-suffix-wave :if-mode dired-mode)]
   ["Text Mode Actions"
    :if-derived text-mode
    ("tw" "wave textually" tsc-suffix-wave)
    ("to" "wave org-modeishly" tsc-suffix-wave :if-mode org-mode)]])

;; (tsc-visibility-predicates)

(defun tsc--switch-on-p ()
  (transient-arg-value
   "--switch"
   (transient-args transient-current-command)))

(transient-define-prefix tsc-inapt ()
  "Prefix that configures child with inapt predicates."
  :refresh-suffixes t ; important for updating inapt! (1)
  ["Options"
   ("-s" "switch" "--switch"
    ;; we want to see the most recent value in `transient-args' (2)
    :transient transient--do-call)]

  ["Appropriate Suffixes"
   ("s" "switched" tsc--wave-switchedly
    :transient t
    :inapt-if-not tsc--switch-on-p)
   ("u" "unswitched" tsc--wave-unswitchedly
    :transient t
    :inapt-if tsc--switch-on-p)]

  ["Appropiate Group"
   :inapt-if-not tsc--switch-on-p
   ("q" "query" tsc--wave-inquisitively)
   ("w" "write" tsc--wave-writingly)])

;; (tsc-inapt)

(transient-define-prefix tsc-levels-and-visibility ()
  "Prefix with visibility levels for hiding rarely used commands."

  [["Setting the Current Level"
    ;; this binding is normally not displayed.  The value of
    ;; `transient-show-common-commands' controls this by default.
    ("C-x l" "set level" transient-set-level)
    ("s" "show level" tsc-suffix-show-level)]

   [2 "Per Group" ;; 1 is the default default-child-level
      ("ws" "wave surely" tsc--wave-surely)
      (3"wn" "wave normally" tsc--wave-normally)
      (5"wb" "wave non-essentially" tsc--wave-non-essentially)]

   [3 "Per Group Somewhat Useful"
      ("wd" "wave definitely" tsc--wave-definitely)]

   [6 "Groups hide visible children"
      (1 "wh" "wave hidden" tsc--wave-hidden)]

   [5 "Per Group Rarely Useful"
      ("we" "wave eventually" tsc--wave-eventually)]])

;; (tsc-levels-and-visibility)

(transient-define-prefix tsc-generated-child ()
  "Prefix that uses `setup-children' to generate single child."

  ["Replace this child"
   ;; Let's override the group's method
   :setup-children
   (lambda (_) ; we don't care about the stupid suffix

     ;; remember to return a list
     (list (transient-parse-suffix
            'transient--prefix
            '("r" "replacement" (lambda ()
                                  (interactive)
                                  (message "okay!"))))))

   ;; This child will not be visible when you run the example because it is
   ;; replaced dynamically when the transient is set up
   ("s" "haha stupid suffix" (lambda ()
                               (interactive)
                               (message "You should replace me!")))])

;; (tsc-generated-child)

(transient-define-prefix tsc-generated-group ()
  "Prefix that uses `setup-children' to generate a group."

  ["Replace this child"
   ;; Let's override the group's method
   :setup-children
   (lambda (_) ; we don't care about the stupid suffix

     ;; the result of parsing here will be a group
     (transient-parse-suffixes
      'transient--prefix
      ["Group Name" ("r" "replacement" (lambda ()
                                         (interactive)
                                         (message "okay!")))]))

   ;; This child will not be visible when you run the example because it is
   ;; replaced dynamically when the transient is set up
   ("s" "haha stupid suffix" (lambda ()
                               (interactive)
                               (message "You should replace me!")))])

;; (tsc-generated-group)

(defun tsc--self-modifying-add-command (command-symbol sequence)
  (interactive "CSelect a command: \nMkey sequence: ")

  ;; Generate an infix that will call the command and add it to the
  ;; second group (index 1 at the 0th position)
  (transient-insert-suffix
    'tsc-self-modifying
    '(0 1 0) ; set the child in `tsc-inception' for help with this argument
    (list sequence (format "Call %s" command-symbol) command-symbol :transient t))

  ;; we must re-enter the transient to force the layout update
  (transient-setup 'tsc-self-modifying))

(transient-define-prefix tsc-self-modifying ()
  "Prefix that uses `transient-insert-suffix' to add commands to itself."

  [["Add New Commands"
    ("a" "add command" tsc--self-modifying-add-command)]
   ["User Defined"
    ""]]) ; blank line suffix creates an insertion point

;; (tsc-self-modifying)

;; The children we will be picking can be of several forms.  The
;; transient--layout symbol property of a prefix is a vector of vectors,
;; lists, and strings.  It's not the actual eieio types or we would use
;; `transient-format-description' to just ask them for the descriptions.
(defun tsc--layout-child-desc (layout-child)
  "Get the description from LAYOUT-CHILD.
LAYOUT-CHILD is a transient layout vector or list."
  (let ((description
         (cond
          ((vectorp layout-child)
           (or (plist-get (aref layout-child 2) :description)
               "<group, no desc>")) ; group
          ((stringp layout-child) layout-child) ; plain-text child
          ((listp layout-child)
           (plist-get (elt layout-child 2) :description)) ; suffix
          (t
           (message
            (propertize "You traversed into a child's list elements!"
                        'face 'warning))
           (format "(child's interior) element: %s" layout-child)))))
    (cond
     ;; The description is sometimes a callable function with no arguments,
     ;; so let's call it in that case.  Note, the description may be
     ;; designed for one point in the transient's lifecycle but we could
     ;; call it in a different one, causing its behavior to change.
     ((functionp description) (apply description))
     (t description))))

;; We repeat the read using a lisp expression from `read-from-minibuffer' to
;; get the LOC key for `transient-get-suffix' until we get a valid result.
;; This ensures we don't store an invalid LOC.
(defun tsc-child-infix--reader (prompt initial-input history)
  "Read a location and check that it exists within the current transient.
PROMPT, INITIAL-INPUT, and HISTORY are forwarded to `read-from-minibuffer'."
  (let ((command (oref transient--prefix command))
        (success nil))
    (while (not success)
      (let* ((loc (read (read-from-minibuffer
                         prompt initial-input nil nil history)))
             (child (ignore-errors (transient-get-suffix command loc))))
        (if child (setq success loc)
          (message (propertize
                    (format
                     "Location could not be found in prefix %s"
                     command)
                    'face 'error))
          (sit-for 3))))
    success))

;; Inherit from variable abstract class
(defclass tsc-child-infix (transient-variable)
  ((value-object :initarg value-object :initform nil)
   ;; this is a new slot for storing the hydrated value.  we re-use the
   ;; value infrastructure for storing the serialization-friendly value,
   ;; which is basically a suffix addres or id.

   (reader :initform #'tsc-child-infix--reader)
   (prompt :initform
           "Location, a key \"c\",\ suffix-command-symbol like\ tsc--wave-normally or coordinates like (0 2 0): ")))

;; We have to define this on non-abstract infix classes.  See
;; `transient-init-value' in transient source.  The method on
;; `transient-argument' class was used to make this example, but it
;; does support a lot of behaviors.  In short, the prefix has a value
;; and you rehydrate the infix by looking into the prefix's value to
;; find the suffix value.  Because our stored value is basically a
;; serialization, we rehydrate it to be sure it's a valid value.
;; Remember to handle values you can't rehydrate.
(cl-defmethod transient-init-value ((obj tsc-child-infix))
  "Set the `value' and `value-object' slots using the prefix's value."

  ;; in the prefix declaration, the initial description is a reliable key
  (let ((variable (oref obj description)))
    (oset obj variable variable)

    ;; rehydrate the value if the prefix has one for this infix
    (when-let* ((prefix-value (oref transient--prefix value))
                ;; (argument (and (slot-boundp obj 'argument)
                ;;   (oref obj argument)))
                (value (cdr (assoc variable prefix-value)))
                ;; rehydrate
                (value-object (transient-get-suffix
                               (oref transient--prefix command) value)))
      (oset obj value value)
      (oset obj value-object value-object))))

(cl-defmethod transient-infix-set ((obj tsc-child-infix) value)
  "Update `value' slot to VALUE.
Update `value-object' slot to the value corresponding to VALUE."
  (let* ((command (oref transient--prefix command))
         (child (ignore-errors (transient-get-suffix command value))))
    (oset obj value-object child)
    (oset obj value (if child value nil)))) ; TODO a bit ugly

;; If you are making a suffix that needs history, you need to define
;; this method.  The example here almost identical to the method
;; defined for `transient-option',
(cl-defmethod transient-infix-value ((obj tsc-child-infix))
  "Return our actual value for rehydration later."

  ;; Note, returning a cons for the value is very flexible and will
  ;; work with homoiconicity in persistence.
  (cons (oref obj variable) (oref obj value)))

;; Show user's a useful representation of your ugly value
(cl-defmethod transient-format-value ((obj tsc-child-infix))
  "All transient children have some description we can display.
Show either the child's description or a default if no child is selected."
  (if-let* ((value (and (slot-boundp obj 'value) (oref obj value)))
            (value-object (and (slot-boundp obj 'value-object)
                               (oref obj value-object))))
      (propertize
       (format "(%s)" (tsc--layout-child-desc value-object))
       'face 'transient-value)
    (propertize "¯\_(ツ)_/¯" 'face 'transient-inactive-value)))

;; Now that we have our class defined, we can create an infix the usual
;; way, just specifying our class
(transient-define-infix tsc--inception-child-infix ()
  :class tsc-child-infix)

;; All set!  This transient just tests our or new toy.
(transient-define-prefix tsc-inception ()
  "Prefix that picks a suffix from its own layout."

  [["Pick a suffix"
    ("-s" "just a switch" "--switch") ; makes history value structure apparent
    ("c" "child" tsc--inception-child-infix)]

   ["Some suffixes"
    ("s" "wave surely" tsc--wave-surely)
    ("d" "wave definitely" tsc--wave-definitely)
    ("e" "wave eventually" tsc--wave-eventually)
    ("C" "call & exit normally" tsc--wave-normally :transient nil)]

   ["Read variables"
    ("r" "read args" tsc-suffix-print-args )]])

;; (tsc-inception)
;;
;; Try setting the infix to "e" (yes, include quotes)
;; Try: (1 2)
;; Try: tsc--wave-normally
;;
;; Observe that the LOC you enter is displayed using the description at that
;; point
;;
;; Set the infix and re-open it with C-x s, C-g, and M-x tsc-inception
;; Observe that the set value persists across invocations
;;
;; Save the infix, with C-x C-s, re-evaluate the prefix, and open the
;; prefix again.
;;
;; Try flipping through history, C-x n, C-x p
;; Now do think of doing things like this with org ids, magit-sections,
;; buffers etc.

(transient-define-suffix tsc--inception-update-description ()
  "Update the description of of the selected child."
  (interactive)
  (let* ((args (transient-args transient-current-command))
         (description (transient-arg-value "--description=" args))
         ;; This is the part where we read the other infix.  It's
         ;; similar to how we find the value during rehydration, but
         ;; hard-coding the infix's argument, "child", which is used
         ;; in its `transient-infix-value' method.
         (loc (cdr (assoc "child" args)))
         (layout-child (transient-get-suffix 'tsc-inception-update loc)))

    ;; Once again, do different bodies based on what we found at the
    ;; layout locition.  This complexity is beacuse of the data we
    ;; are operating on, not the transient methods we needed to
    ;; implement.
    (cond
     ((or (listp layout-child) ; child
          (vectorp layout-child) ; group
          (stringp layout-child)) ; string child
      (if (stringp layout-child) ; plain-text child
          (transient-replace-suffix 'tsc-inception-update loc description)

        (plist-put (elt layout-child 2) :description description)))
     (t (message
         (propertize
          (format "Don't know how to modify whatever is at: %s" loc)
                     'face 'warning))))

    ;; re-enter the transient manually to display the modified layout
    (transient-setup transient-current-command)))

(transient-define-prefix tsc-inception-update ()
  "Prefix that picks and updates its own suffix."

  [["Pick a suffix"
    ("c" "child" tsc--inception-child-infix :argument "child")]

   ["Update the description!"
    ("-d" "description" "--description=")
    ("u" "update" tsc--inception-update-description
     :transient transient--do-exit)]

   ["Some suffixes"
    ("s" "wave surely" tsc--wave-surely)
    ("d" "wave definitely" tsc--wave-definitely)
    ("e" "wave eventually" tsc--wave-eventually)
    ("C" "call & exit normally" tsc--wave-normally :transient nil)]

   ["Read variables"
    ("r" "read args" tsc-suffix-print-args )]])

;; (tsc-inception-update)
;;
;; 1. Press 'c' to start picking a suffix.  For example, enter the string "e"
;; 2. Press 'C-x s' to set the values of this transient for the future
;; 3. Then set the description, anything, no quotes
;; 4. Then press 'u' the suffix's you picked with the new description!
;;
;; Using a transient to modify a transient (⊃｡•́‿•̀｡)⊃━✿✿✿✿✿✿
;;
;; Observe that the set values are persisted across invocations.
;; Saving also works.  This makes it easier to set the description
;; multiple times in succession.  The Payoff when building larger
;; applications like magit rapidly adds up.

(transient-define-prefix tsc-showcase ()
  "A launcher for a currated selection of examples.
While most of the prefixes have their :transient slot set to t, it's not
possible to return from all of them, especially if they demonstrate flow
control such as replacing or exiting."

  [["Layouts"
    ("ls" "stacked" tsc-layout-stacked :transient t)
    ("lc" "columns" tsc-layout-columns :transient t)
    ("lt" "stacked columns" tsc-layout-stacked-columns :transient t)
    ("lg" "grid" tsc-layout-the-grid :transient t)
    ("lp" "spaced out" tsc-layout-spaced-out :transient t)
    ("le" "explicit class" tsc-layout-explicit-classes :transient t)
    ("ld" "descriptions" tsc-layout-descriptions :transient t)
    ;; padded description to sc
    ("lD" "dynamic descriptions            "
     tsc-layout-dynamic-descriptions :transient t)]

   ["Nesting & Flow Control"
    ("fs" "stay transient" tsc-stay-transient :transient t)
    ("fb" "binding sub-prefix" tsc-simple-parent :transient t)
    ("fr" "sub-prefix with return" tsc-simple-parent-with-return :transient t)
    ("fm" "manual setup in suffix" tsc-parent-with-setup-suffix :transient t)
    ("fi" "mixing interactive" tsc-interactive :transient t)
    ("fe" "early return" tsc-simple-messager :transient t)]]

  ["State Management"
   ["Transient State & Peristence"
    ("sb" "a bunch of infixes" tsc-basic-infixes :transient t)
    ("sc" "using scope (accepts prefix arg)" tsc-scope :transient t)
    ("sn" "set & save / snowcones" tsc-snowcone-eater :transient t)
    ("sp" "history key / ping-pong" tsc-ping :transient t)
    ("sg" "always forget / goldfish" tsc-goldfish :transient t)
    ("se" "always remember / elephant" tsc-elephant :transient t)
    ("sd" "default values" tsc-default-values :transient t)
    ("sf" "enforcing inputs" tsc-enforcing-inputs :transient t)
    ("sl" "lisp variables" tsc-lisp-variable :transient t)]
   ["CLI arguments"
    ("cb" "basic arguments" tsc-switches-and-arguments :transient t)
    ("cm" "random-init infix" tsc-maybe-on :transient t)
    ("cc" "basic choices" tsc-animal-choices :transient t)
    ("ce" "exclusive switches" tsc-exclusive-switches :transient t)
    ("ci" "incompatible switches" tsc-incompatible :transient t)
    ("co" "completions for choices" tsc-choices-with-completions :transient t)
    ("cc" "cowsay cli wrapper" tsc-cowsay :transient t)]
   ["Ad-Hoc Vanilla Elisp" ; padded right in layouts
    ("ev" "display & update defvar" tsc-defvar-settings)
    ("el" "buffer local values" tsc-buffer-local)
    ("ef" "file based persistence" tsc-persistent-settings)]]

  [["Visibility"
    ;; padded description to sc
    ("vp" "predicates                      "
     tsc-visibility-predicates :transient t)
    ("vi" "inapt (not suitable)" tsc-inapt :transient t)
    ("vl" "levels" tsc-levels-and-visibility :transient t)]

   ["Advanced"
    ("ac" "generated child" tsc-generated-child :transient t)
    ("ag" "generated group" tsc-generated-group :transient t)
    ("as" "self-modifying" tsc-self-modifying :transient t)
    ("ai" "custom infixes" tsc-inception :transient t)
    ("au" "custom infixes & update" tsc-inception-update :transient t)]])

(provide 'transient-showcase)
;;; transient-showcase.el ends here
