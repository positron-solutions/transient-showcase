;;; transient-showcase.el --- transient features & behavior showcase -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Positron Solutions


;; Author: Psionik K <73710933+psionic-k@users.noreply.github.com>
;; Keywords: convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Homepage: http://github.com/positron-solutions/transient-showcase

;;; License notice:

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This package is created from the README and serves as a fast way to load
;; all of the examples without tangling the org document.  This is appropriate
;; if you just want to quickly browse through the examples and see their
;; source code.
;;
;; M-x transient-showcase contains most of the prefixes and can be bound for
;; use as a quick reference.  Just use transient's help for each command to
;; see the source. C-h <suffix key>.
;;

;;; Code:

(require 'transient)


(defun ts-suffix-wave ()
  "Wave at the user"
  (interactive)
  (message "Waves at the user at: %s." (current-time-string)))


(defvar ts-busy nil "Are we busy?")

(defun ts--busy-p () "Are we busy?" ts-busy)

(transient-define-suffix ts--toggle-busy ()
  "Toggle busy"
  (interactive)
  (setf ts-busy (not ts-busy))
  (message (propertize (format "busy: %s" ts-busy)
                       'face 'success)))


(transient-define-suffix ts-suffix-show-level ()
  "Show the current transient's level."
  :transient t
  (interactive)
  (message "Current level: %s" (oref transient-current-prefix level)))


;; Because command names are used to store and lookup child levels, we have
;; define a macro to generate unqiquely named wavers.  See #153 at
;; https://github.com/magit/transient/issues/153
(defmacro ts--define-waver (name)
  "Define a new suffix named ts--wave-NAME"
  `(transient-define-suffix ,(intern (format "ts--wave-%s" name)) ()
     ,(format "Wave at the user %s" name)
     :transient t
     (interactive)
     (message (format "Waves at %s" (current-time-string)))))

;; Each form results in a unique suffix definition.
(ts--define-waver "surely")
(ts--define-waver "normally")
(ts--define-waver "non-essentially")
(ts--define-waver "definitely")
(ts--define-waver "eventually")
(ts--define-waver "hidden")


(transient-define-suffix ts-suffix-print-args (prefix-arg)
  "Report the universal argument, prefix's scope, and infix values."
  :transient 'transient--do-call
  (interactive "P")
  (let ((args (transient-args (oref transient-current-prefix command)))
        (scope (oref transient-current-prefix scope)))
    (message "prefix-arg: %s \nprefix's scope value: %s \ntransient-args: %s"
             prefix-arg scope args)))
(transient-define-prefix ts-hello ()
  "Prefix that is minimal and uses an anonymous command suffix."
  [("s" "call suffix"
    (lambda ()
      (interactive)
      (message "Called a suffix")))])

;; First, use M-x org-babel-execute-src-blk to cause `ts-hello' to be defined
;; Second, M-x `eval-last-sexp' with your point at the end of the line below
;; (ts-hello)
(transient-define-suffix ts-suffix-wave-macroed ()
  "Prefix that waves with macro-defined suffix."
  :transient t
  :key "T"
  :description "wave from macro definition"
  (interactive)
  (message "Waves from a macro definition at: %s" (current-time-string)))
;; ts-suffix-wave-suffix defined above

(transient-define-prefix ts-wave-macro-defined ()
  "Prefix to wave using a macro-defined suffix"
  [(ts-suffix-wave-macroed)]) ; note, information moved from prefix to the suffix.

;; (ts-wave-macro-defined)
(defun ts--wave-override ()
  "Vanilla command used to override suffix's commands."
  (interactive)
  (message "This suffix was overridden.  I am what remains."))

(transient-define-prefix ts-wave-overridden ()
  "Prefix that waves with overridden suffix behavior"
  [(ts-suffix-wave-macroed
    :transient nil
    :key "O"
    :description "wave overridingly"
    :command ts--wave-override)]) ; we overrode what the suffix even does

;; (ts-wave-overridden)
(transient-define-prefix ts-layout-stacked ()
  "Prefix with layout that stacks groups on top of each other."
  ["Top Group" ("wt" "wave top" ts-suffix-wave)]
  ["Bottom Group" ("wb" "wave bottom" ts-suffix-wave)])

;; (ts-layout-stacked)
(transient-define-prefix ts-layout-columns ()
  "Prefix with side-by-side layout."
  [["Left Group" ("wl" "wave left" ts-suffix-wave)]
   ["Right Group" ("wr" "wave right" ts-suffix-wave)]])

;; (ts-layout-columns)
(transient-define-prefix ts-layout-stacked-columns ()
  "Prefix with stacked columns layout."
  ["Top Group"
   ("wt" "wave top" ts-suffix-wave)]

  [["Left Group"
    ("wl" "wave left" ts-suffix-wave)]
   ["Right Group"
    ("wr" "wave right" ts-suffix-wave)]])

;; (ts-layout-stacked-columns)
(transient-define-prefix ts-layout-spaced-out ()
  "Prefix lots of spacing for users to space out at."
  ["" ; cannot add another empty string because it will mix suffixes with groups
   ["Left Group"
    ""
    ("wl" "wave left" ts-suffix-wave)
    ("L" "wave lefter" ts-suffix-wave)
    ""
    ("bl" "wave bottom-left" ts-suffix-wave)
    ("z" "zone\n" zone)] ; the newline does pad

   [[]] ; empty vector will do nothing

   [""] ; vector with just empty line has no effect

   ;; empty group will be ignored
   ;; (useful for hiding in dynamic layouts)
   ["Empty Group\n"]

   ["Right Group"
    ""
    ("wr" "wave right" ts-suffix-wave)
    ("R" "wave righter" ts-suffix-wave)
    ""
    ("br" "wave bottom-right" ts-suffix-wave)]])

;; (ts-layout-spaced-out)
(transient-define-prefix ts-layout-explicit-classes ()
  "Prefix with group class used to explicitly specify layout."
  [:class transient-row "Row"
          ("l" "wave left" ts-suffix-wave)
          ("r" "wave right" ts-suffix-wave)]
  [:class transient-column "Column"
          ("t" "wave top" ts-suffix-wave)
          ("b" "wave bottom" ts-suffix-wave)])

;; (ts-layout-explicit-classes)
(transient-define-prefix ts-layout-the-grid ()
  "Prefix with groups in a grid-like arrangement."

  [:description "The Grid\n" ; must use slot or macro is confused
   ["Left Column" ; note, no newline
    ("ltt" "left top top" ts-suffix-wave)
    ("ltb" "left top bottom" ts-suffix-wave)
    ""
    ("lbt" "left bottom top" ts-suffix-wave)
    ("lbb" "left bottom bottom" ts-suffix-wave)] ; note, no newline

   ["Right Column\n"
    ("rtt" "right top top" ts-suffix-wave)
    ("rtb" "right top bottom" ts-suffix-wave)
    ""
    ("rbt" "right bottom top" ts-suffix-wave)
    ("rbb" "right bottom bottom\n" ts-suffix-wave)]])

;; (ts-layout-the-grid)
(transient-define-prefix ts-layout-descriptions ()
  "Prefix with descriptions specified with slots."
  ["Let's Give This Transient a Title\n" ; yes the newline works
   ["Group One"
    ("wo" "wave once" ts-suffix-wave)
    ("wa" "wave again" ts-suffix-wave)]

   ["Group Two"
    ("ws" "wave some" ts-suffix-wave)
    ("wb" "wave better" ts-suffix-wave)]]

  ["Bad title" :description "Group of Groups"
   ["Group Three"
    ("k" "bad desc" ts-suffix-wave :description "key-value wins")
    ("n" ts-suffix-wave :description "no desc necessary")]
   [:description "Key Only Def"
    ("wt" "wave too much" ts-suffix-wave)
    ("we" "wave excessively" ts-suffix-wave)]])

;; (ts-layout-descriptions)
(transient-define-prefix ts-layout-dynamic-descriptions ()
   "Prefix that generate descriptions dynamically when transient is shown."
   ;; group using function-name to generate description
   [:description current-time-string
    ;; single suffix with dynamic description
    ("wa" ts-suffix-wave
     :description (lambda ()
                    (format "Wave at %s" (current-time-string))))]
   ;; group with anonymoous function generating description
   [:description (lambda ()
                   (format "Group %s" (org-id-new)))
                 ("wu" "wave uniquely" ts-suffix-wave)])

;; (ts-layout-dynamic-descriptions)
(transient-define-prefix ts-stay-transient ()
  "Prefix where some suffixes do not exit."
  ["Exit or Not?"

   ;; this suffix will not exit after calling sub-prefix
   ("we" "wave & exit" ts-wave-overridden)
   ("ws" "wave & stay" ts-wave :transient t)])

;; (ts-stay-transient)
(transient-define-prefix ts--simple-child ()
  ["Simple Child"
   ("wc" "wave childishly" ts-suffix-wave)])

(transient-define-prefix ts-simple-parent ()
  "Prefix that calls a child prefix."
  ["Simple Parent"
   ("w" "wave parentally" ts-suffix-wave)
   ("b" "become child" ts--simple-child)])

;; (ts-simple--child)
;; (ts-simple-parent)
(transient-define-prefix ts-simple-parent-with-return ()
  "Prefix with a child prefix that returns."
  ["Parent With Return"
   ("w" "wave parentally" ts-suffix-wave)
   ("b" "become child with return" ts-simple-child :transient t)])

;; Child does not "return" when called independently
;; (ts-simple-child)
;; (ts-simple-parent-with-return)
(transient-define-suffix ts-suffix-setup-child ()
  "A suffix that uses `transient-setup' to manually load another transient."
  (interactive)
  ;; note that it's usually during the post-command side of calling the
  ;; command that the actual work to set up the transient will occur.
  ;; This is an implementation detail because it depends if we are calling
  ;; `transient-setup' while already transient or not.
  (transient-setup 'ts-simple-child))

(transient-define-prefix ts-parent-with-setup-suffix ()
  "Prefix with a suffix that calls `transient-setup'."
  ["Simple Parent"
   ("wp" "wave parentally" ts-suffix-wave :transient t) ; remain transient

   ;; You may need to specify a different pre-command (the :transient) key
   ;; because we need to clean up this transient or create some conditions
   ;; to trigger the following transient correctly.  This example will
   ;; work with `transient--do-replace' or no custom pre-command

   ("bc" "become child" ts-suffix-setup-child :transient transient--do-replace)])

;; (ts-parent-with-setup-suffix)
(transient-define-suffix ts--suffix-interactive-string (user-input)
  "An interactive suffix that obtains string input from the user."
  (interactive "sPlease just tell me what you want!: ")
  (message "I think you want: %s" user-input))

(transient-define-suffix ts--suffix-interactive-buffer-name (buffer-name)
  "An interactive suffix that obtains a buffer name from the user."
  (interactive "b")
  (message "You selected: %s" buffer-name))

(transient-define-prefix ts-interactive-basic ()
  "Prefix with interactive user input."
  ["Interactive Command Suffixes"
   ("s" "enter string" ts--suffix-interactive-string)
   ("b" "select buffer" ts--suffix-interactive-buffer-name)])

;; (ts-interactive-basic)
;; infix defined with a macro
(transient-define-argument ts--exclusive-switches ()
  "This is a specialized infix for only selecting one of several values."
  :class 'transient-switches
  :argument-format "--%s-snowcone"
  :argument-regexp "\\(--\\(grape\\|orange\\|cherry\\|lime\\)-snowcone\\)"
  :choices '("grape" "orange" "cherry" "lime"))

(transient-define-prefix ts-basic-infixes ()
  "Prefix that just shows off many typical infix types."
  ["Infixes"

   ;; from macro
   ("-e" "exclusive switches" ts--exclusive-switches)

   ;; shorthand definitions
   ("-b" "switch with shortarg" ("-w" "--switch-short")) ; with :short-arg != :key
   ("-s" "switch" "--switch")
   ( "n" "no dash switch" "still works")
   ("-a" "argument" "--argument=" :prompt "Let's argue because: ")

   ;; a bit of inline EIEIO in our shorthand
   ("-n" "never empty" "--non-null=" :always-read t
    :init-value (lambda (obj) (oset obj value "better-than-nothing")))

   ("-c" "choices" "--choice=" :choices (foo bar baz))]

  ["Show Args"
   ("s" "show arguments" ts-suffix-print-args)])

;; (ts-basic-infixes)
(transient-define-suffix ts--read-prefix-scope ()
  "Read the scope of the prefix."
  :transient 'transient--do-call
  (interactive)
  (let ((scope (oref transient-current-prefix scope)))
    (message "scope: %s" scope)))

(transient-define-suffix ts--double-scope-re-enter ()
  "Re-enter the current prefix with double the scope."
  ;; :transient 'transient--do-replace ; builds up the stack
  :transient 'transient--do-exit
  (interactive)
  (let ((scope (oref transient-current-prefix scope)))
    (if (numberp scope)
        (transient-setup transient-current-command nil nil :scope (* scope 2))
      (message (propertize (format "scope was non-numeric! %s" scope) 'face 'warning))
      (transient-setup transient-current-command))))

(transient-define-suffix ts--update-scope-with-prefix-re-enter (new-scope)
  "Re-enter the prefix with double the scope."
  ;; :transient 'transient--do-replace ; builds up the stack
  :transient 'transient--do-exit ; do not build up the stack
  (interactive "P")
  (message "universal arg: %s" new-scope)
  (transient-setup transient-current-command nil nil :scope new-scope))

(transient-define-prefix ts-scope (scope)
  "Prefix demonstrating use of scope."

  ;; note!  this is a location where we definitely had to use
  ;; `transient--prefix' or get the transient object from the ts-scope symbol.
  ;; `transient-current-prefix' is not correct here!
  [:description (lambda () (format "Scope: %s" (oref transient--prefix scope)))
   [("r" "read scope" ts--read-prefix-scope)
    ("d" "double scope" ts--double-scope-re-enter)
    ("o" "update scope (use prefix argument)" ts--update-scope-with-prefix-re-enter)]]
  (interactive "P")
  (transient-setup 'ts-scope nil nil :scope scope))

;; Setting an interactive argument for `eval-last-sexp' is a little different
;; (let ((current-prefix-arg 4)) (call-interactively 'ts-scope))

;; (ts-scope)
;; Then press "C-u 4 o" to update the scope
;; Then d to double
;; Then r to read
;; ... and so on
;; C-g to exit
(transient-define-suffix ts-suffix-eat-snowcone (args)
  "Eat the snowcone!
This command can be called from it's parent, `ts-snowcone-eater' or independently."
  :transient t
  ;; you can use the interactive form of a command to obtain a default value
  ;; from the user etc if the one obtained from the parent is invalid.
  (interactive (list (transient-args 'ts-snowcone-eater)))

  ;; `transient-arg-value' can (with varying success) pick out individual
  ;; values from the results of `transient-args'.

  (let ((topping (transient-arg-value "--topping=" args))
        (flavor (transient-arg-value "--flavor=" args)))
    (message "I ate a %s flavored snowcone with %s on top!" flavor topping)))

(transient-define-prefix ts-snowcone-eater ()
  "Prefix demonstrating set & save infix persistence."

  ;; This prefix has a default value that ts-suffix-eat-snowcone can see
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
    (lambda () (interactive) (message "saved!") (transient-save)) :transient t)
   ("R" "reset snowcone settings"
    (lambda () (interactive) (message "reset!") (transient-reset)) :transient t)]

  ["Actions"
   ("m" "message arguments" ts-suffix-print-args)
   ("e" "eat snowcone" ts-suffix-eat-snowcone)])

;; First call will use the transient's default value
;; M-x ts-suffix-eat-snowcone or `eval-last-sexp' below
;; (call-interactively 'ts-suffix-eat-snowcone)
;; (ts-snowcone-eater)
;; Eat some snowcones with different flavors
;; ...
;; ...
;; ...
;; Now save the value and exit the transient.
;; When you call the suffix independently, it can still read the saved values!
;; M-x ts-suffix-eat-snowcone or `eval-last-sexp' below
;; (call-interactively 'ts-suffix-eat-snowcone)
(transient-define-prefix ts-ping ()
  "Prefix demonstrating history sharing."

  :history-key 'non-unique-name

  ["Ping"
   ("-g" "game" "--game=")
   ("p" "ping the pong" ts-pong)
   ("a" "print args" ts-suffix-print-args :transient nil)])

(transient-define-prefix ts-pong ()
  "Prefix demonstrating history sharing."

  :history-key 'non-unique-name

  ["Pong"
   ("-g" "game" "--game=")
   ("p" "pong the ping" ts-ping)
   ("a" "print args" ts-suffix-print-args :transient nil)])

;; (ts-ping)
;; Okay here's where it gets weird
;; 1.  Set the value of game to something and remember it
;; 2.  Press a to print the args
;; 3.  Re-open ts-ping.
;; 4.  C-x p to load the previous history, see the old value?
;; 5.  p to switch to the ts-pong transient
;; 6.  C-x p to load the previous history, see the old value from ts-ping???
;; 7. Note that ts-pong uses the same history as ts-ping!
(transient-define-prefix ts-goldfish ()
  "A prefix that cannot remember anything."
  ["Goldfish"
   ("-r" "rememeber" "--i-remember="
    :unsavable t ; infix isn't saved
    :always-read t ; infix always asks for new value
    ;; overriding the method to provide a starting value
    :init-value (lambda (obj) (oset obj value "nothing")))
   ("a" "print args" ts-suffix-print-args :transient nil)])

;; (ts-goldfish)
(transient-define-suffix ts-suffix-remember-and-wave ()
  "Wave, and force the prefix to set it's saveable infix values."
  (interactive)

  ;; (transient-reset) ; forget
  (transient-set) ; save for this session
  ;; If you combine reset with set, you get a reset for future sessions only.
  ;; (transient-save) ; save for this and future sessions
  ;; (transient-reset-value some-other-prefix-object)

  (message "Waves at user at: %s.  You will never be forgotten." (current-time-string)))

(transient-define-prefix ts-elephant ()
  "A prefix that always remembers its infixes."
  ["Elephant"
   ("-r" "rememeber" "--i-remember="
    :always-read t)
   ("w" "remember and wave" ts-suffix-remember-and-wave)
   ("a" "print args (skips remembering)" ts-suffix-print-args :transient nil)])

;; (ts-elephant)
(transient-define-prefix ts-default-values ()
  "A prefix with a default value."

  :value '("--toggle" "--value=5")

  ["Arguments"
   ("t" "toggle" "--toggle")
   ("v" "value" "--value=" :prompt "an integer: ")]

  ["Show Args"
   ("s" "show arguments" ts-suffix-print-args)])

;; (ts-default-values)
(transient-define-prefix ts-enforcing-inputs ()
  "A prefix with enforced input type."

  ["Arguments"
   ("v" "value" "--value=" :prompt "an integer: " :reader transient-read-number-N+)]

  ["Show Args"
   ("s" "show arguments" ts-suffix-print-args)])

;; (ts-enforcing-inputs)
(defvar ts--position '(0 0) "A transient prefix location")

  (transient-define-infix ts--pos-infix ()
    "A location, key, or command symbol"
    :class 'transient-lisp-variable
    :transient t
    :prompt "An expression such as (0 0), \"p\", nil, 'ts--msg-pos: "
    :variable 'ts--position)

  (transient-define-suffix ts--msg-pos ()
    "Message the element at location"
    :transient 'transient--do-call
    (interactive)
    ;; lisp variables are not sent in the usual (transient-args) list.
    ;; Just read `ts--position' directly.
    (let ((suffix (transient-get-suffix transient-current-command ts--position)))
      (message "%s" (oref suffix description))))

  (transient-define-prefix ts-lisp-variable ()
    "A prefix that updates and uses a lisp variable."
    ["Location Printing"
     [("p" "position" ts--pos-infix)]
     [("m" "message" ts--msg-pos)]])

  ;; (ts-lisp-variable)
(transient-define-prefix ts-switches-and-arguments (arg)
  "A prefix with switch and argument examples."
  [["Arguments"
    ("-s" "switch" "--switch")
    ("-a" "argument" "--argument=")
    ("t" "toggle" "--toggle")
    ("v" "value" "--value=")]

   ["More Arguments"
    ("-f" "argument with forced class" "--forced-class " :class transient-option)
    ("I" "argument with inline" ("-i" "--inline-shortarg="))
    ("S" "inline shortarg switch" ("-n" "--inline-shortarg-switch"))]]

  ["Commands"
   ("w" "wave some" ts-wave)
   ("s" "show arguments" ts-suffix-print-args)]) ; use to analyze the switch values

;; (ts-switches-and-arguments)
(transient-define-infix ts--random-init-infix ()
  "Switch on and off"
  :argument "--switch"
  :shortarg "-s" ; will be used for :key when key is not set
  :description "switch"
  :init-value (lambda (obj)
                (oset obj value
                      (eq 0 (random 2))))) ; write t with 50% probability

(transient-define-prefix ts-maybe-on ()
  "A prefix with a randomly intializing switch."
  ["Arguments"
   (ts--random-init-infix)]
  ["Show Args"
   ("s" "show arguments" ts-suffix-print-args)])

;; (ts-maybe-on)
;; (ts-maybe-on)
;; ...
;; Run the command a few times to see the random initialization of `ts--random-init-infix'
;; It will only take more than ten tries for one in a thousand users.  Good luck.
(transient-define-argument ts--animals-argument ()
  "Animal picker"
  :argument "--animals="
  ; :multi-value t ; multi-value can be set to --animals=fox,otter,kitten etc
  :class 'transient-option
  :choices '("fox" "kitten" "peregrine" "otter"))

(transient-define-prefix ts-animal-choices ()
  "Prefix demonstrating selecting animals from choices."
  ["Arguments"
   ("-a" "--animals=" ts--animals-argument)]
  ["Show Args"
   ("s" "show arguments" ts-suffix-print-args)])

;; (ts-animal-choices)
(transient-define-argument ts--snowcone-flavor ()
  :description "Flavor of snowcone"
  :class 'transient-switches
  :key "f"
  :argument-format "--%s-snowcone"
  :argument-regexp "\\(--\\(grape\\|orange\\|cherry\\|lime\\)-snowcone\\)"
  :choices '("grape" "orange" "cherry" "lime"))

(transient-define-prefix ts-exclusive-switches ()
  "Prefix demonstrating exclusive switches."
  :value '("--orange-snowcone")

  ["Arguments"
   (ts--snowcone-flavor)]
  ["Show Args"
   ("s" "show arguments" ts-suffix-print-args)])

;; (ts-exclusive-switches)
(transient-define-prefix ts-incompatible ()
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
   ("s" "show arguments" ts-suffix-print-args)])

;; (ts-incompatible)
(defun ts--quit-cowsay ()
  "Kill the cowsay buffer and exit"
  (interactive)
  (kill-buffer "*cowsay*"))

(defun ts--cowsay-buffer-exists-p ()
  (not (equal (get-buffer "*cowsay*") nil)))

(transient-define-suffix ts--cowsay-clear-buffer (&optional buffer)
  "Delete the *cowsay* buffer.  Optional BUFFER name."
  :transient 'transient--do-call
  :if 'ts--cowsay-buffer-exists-p
  (interactive) ; todo look at "b" interactive code

  (save-excursion
    (let ((buffer (or buffer "*cowsay*")))
      (set-buffer buffer)
      (delete-region 1 (+ 1 (buffer-size))))))

(transient-define-suffix ts--cowsay (&optional args)
  "Run cowsay"
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

    (when (ts--cowsay-buffer-exists-p)
      (ts--cowsay-clear-buffer))
    (apply #'call-process "cowsay" nil buffer nil args)
    (switch-to-buffer buffer)))

(transient-define-prefix ts-cowsay ()
  "Say things with animals!"

  ; only one kind of eyes is meaningful at a time
  :incompatible '(("-b" "-g" "-p" "-s" "-t" "-w" "-y"))

  ["Message"
   ("m" "message" "--message=" :always-read t)] ; always-read, so clear by entering empty string
  [["Built-in Eyes"
    ("b" "borg" "-b")
    ("g" "greedy" "-g")
    ("p" "paranoid" "-p")
    ("s" "stoned" "-s")
    ("t" "tired" "-t")
    ("w" "wired" "-w")
    ("y" "youthful" "-y")]
   ["Actions"
    ("c" "cowsay" ts--cowsay :transient transient--do-call)
    ""
    ("d" "delete buffer" ts--cowsay-clear-buffer)
    ("q" "quit" ts--quit-cowsay)]])

;; (ts-cowsay)
(transient-define-prefix ts-visibility-predicates ()
  "Prefix with visibility predicates.
Try opening this prefix in buffers with modes deriving from different
abstract major modes."
  ["Empty Groups Not Displayed"
   ;; in org mode for example, this group doesn't appear.
   ("we" "wave elisp" ts-suffix-wave :if-mode emacs-lisp-mode)
   ("wc" "wave in C" ts-suffix-wave :if-mode cc-mode)]

  ["Lists of Modes"
   ("wm" "wave multiply" ts-suffix-wave :if-mode (dired-mode gnus-mode))]

  [["Function Predicates"
    ;; note, after toggling, the transient needs to be re-displayed for the
    ;; predicate to take effect
    ("b" "toggle busy" ts--toggle-busy)
    ("bw" "wave busily" ts-suffix-wave :if ts--busy-p)]

   ["Programming Actions"
    :if-derived prog-mode
    ("pw" "wave programishly" ts-suffix-wave)
    ("pe" "wave in elisp" ts-suffix-wave :if emacs-lisp-mode)]
   ["Special Mode Actions"
    :if-derived special-mode
    ("sw" "wave specially" ts-suffix-wave)
    ("sd" "wave dired" ts-suffix-wave :if-mode dired-mode)]
   ["Text Mode Actions"
    :if-derived text-mode
    ("tw" "wave textually" ts-suffix-wave)
    ("to" "wave org-modeishly" ts-suffix-wave :if-mode org-mode)]])

;; (ts-visibility-predicates)
(defun ts--child-scope-p ()
  "Returns the scope of the current transient.
When this is called in layouts, it's the transient being layed out"
  (let ((scope (oref transient--prefix scope)))
    (message "The scope is: %s" scope)
    scope))

;; the wave suffixes were :transient t as defined, so we need to manually
;; override them to the `transient--do-return' value for :transient slot so
;; that they return back to the parent.
(transient-define-prefix ts--inapt-children ()
  "Prefix with children using inapt predicates."
  ["Inapt Predicates Child"
   ("s" "switched" ts--wave-surely
    :transient transient--do-return
    :if ts--child-scope-p)
   ("u" "unswitched" ts--wave-normally
    :transient transient--do-return
    :if-not ts--child-scope-p)]

  ;; in the body, we read the value of the parent and set our scope to
  ;; non-nil if the switch is set
  (interactive)
  (let ((scope (transient-arg-value "--switch" (transient-args 'ts-inapt-parent))))
    (message "scope: %s" scope)
    (message "type: %s" (type-of scope))
    (transient-setup 'ts--inapt-children nil nil :scope (if scope t nil))))

(transient-define-prefix ts-inapt-parent ()
  "Prefix that configures child with inapt predicates."

  [("-s" "switch" "--switch")
   ("a" "show arguments" ts-suffix-print-args)
   ("c" "launch child prefix" ts--inapt-children :transient transient--do-recurse)])

;; (ts-inapt-parent)
(transient-define-prefix ts-levels-and-visibility ()
  "Prefix with visibility levels for hiding rarely used commands."

  [["Setting the Current Level"
    ;; this binding is normally not displayed.  The value of
    ;; `transient-show-common-commands' controls this by default.
    ("C-x l" "set level" transient-set-level)
    ("s" "show level" ts-suffix-show-level)]

   [2 "Per Group" ; 1 is the default default-child-level
      ("ws" "wave surely" ts--wave-surely) ; 1 is the default default-child-level
      (3"wn" "wave normally" ts--wave-normally)
      (5"wb" "wave non-essentially" ts--wave-non-essentially)]

   [3 "Per Group Somewhat Useful"
      ("wd" "wave definitely" ts--wave-definitely)]

   [6 "Groups hide visible children"
      (1 "wh" "wave hidden" ts--wave-hidden)]

   [5 "Per Group Rarely Useful"
      ("we" "wave eventually" ts--wave-eventually)]])

;; (ts-levels-and-visibility)
(transient-define-prefix ts-generated-child ()
  "Prefix that uses `setup-children' to generate single child."

  ["Replace this child"
   ;; Let's override the group's method
   :setup-children
   (lambda (_) ; we don't care about the stupid suffix

     ;; remember to return a list
     (list (transient-parse-suffix
            transient--prefix
            '("r" "replacement" (lambda ()
                                  (interactive)
                                  (message "okay!"))))))

   ("s" "haha stupid suffix" (lambda ()
                               (interactive)
                               (message "You should replace me!")))])

;; (ts-generated-child)
;; The children we will be picking can be of several forms.  The
;; transient--layout symbol property of a prefix is a vector of vectors, lists,
;; and strings.  It's not the actual eieio types or we would use
;; `transient-format-description' to just ask them for the descriptions.
(defun ts--layout-child-desc (layout-child)
  "Get the description from a transient layout vector or list."
  (let ((description
         (cond
          ((vectorp layout-child) (or (plist-get (aref layout-child 2) :description) "<group, no desc>")) ; group
          ((stringp layout-child) layout-child) ; plain-text child
          ((listp layout-child) (plist-get (elt layout-child 2) :description)) ; suffix
          (t (message (propertize "You traversed into a child's list elements!" 'face 'warning))
             (format "(child's interior) element: %s" layout-child)))))
    (cond
     ;; The description is sometimes a callable function with no arguments,
     ;; so let's call it in that case.  Note, the description may be
     ;; designed for one point in the transient's lifecycle but we could
     ;; call it in a different one, causing its behavior to change.
     ((functionp description) (apply description))
     (t description))))

;; We repeat the read using a lisp expression from `read-from-minibuffer' to get
;; the LOC key for `transient-get-suffix' until we get a valid result.  This
;; ensures we don't store an invalid LOC.
(defun ts-child-infix--reader (prompt initial-input history)
  "Read a location and check that it exists within the current transient."
  (let ((command (oref transient--prefix command))
        (success nil))
    (while (not success)
      (let* ((loc (read (read-from-minibuffer prompt initial-input nil nil history)))
             (child (ignore-errors (transient-get-suffix command loc))))
        (if child (setq success loc)
          (message (propertize
            (format
             "Location could not be found in prefix %s"
             command) 'face 'error)) (sit-for 3))))
    success))

;; Inherit from variable abstract class
(defclass ts-child-infix (transient-variable)
  ((value-object :initarg value-object :initform nil)
   ;; this is a new slot for storing the hydrated value.  we re-use the
   ;; value infrastructure for storing the serialization-friendly value,
   ;; which is basically a suffix addres or id.

   (reader :initform #'ts-child-infix--reader)
   (prompt :initform "Location, a key \"c\", suffix-command-symbol like ts--wave-normally or coordinates like (0 2 0): ")))

;; We have to define this on non-abstract infix classes.  See
;; `transient-init-value' in transient source.  The method on
;; `transient-argument' class is the best example for initializing your
;; suffix based on the prefix's value, but it does support a lot of
;; behaviors.
(cl-defmethod transient-init-value ((obj ts-child-infix))
  "Set the value and object-value using the prefix's value."
  (let* ((prefix-value (oref transient--prefix value))
         (key (oref obj command))
         (value (car (alist-get key prefix-value))) ; car?
         (value-object (transient-get-suffix (oref transient--prefix command) value)))
    (oset obj value value)
    (oset obj value-object value-object)))

(cl-defmethod transient-infix-set ((obj ts-child-infix) value)
  "When the `value' is updated, update the `value-object' as well."
  (let* ((command (oref transient--prefix command))
         (child (ignore-errors (transient-get-suffix command value))))
    (oset obj value-object child)
    (oset obj value (if child value nil))))

;; If you are making a suffix that needs history, you need to define this
;; method.  You also need this method if your value needs some processing
;; or use of an alternate value for later rehydration.  Tell the prefix
;; what to store when setting / saving
(cl-defmethod transient-infix-value ((obj ts-child-infix))
  "Return our actual value for rehydration later."

  ;; this is almost identical to the method defined for `transient-infix',
  ;; but don't forget this if you want history on a suffix for example.
  (list (oref obj command) (oref obj value)))

;; Show user's a useful representation of your ugly value
(cl-defmethod transient-format-value ((obj ts-child-infix))
  "All transient children have some description we can display.
Show either the child's description or a default if no child is selected."
  (if-let* ((value (and (slot-boundp obj 'value) (oref obj value)))
            (value-object (and (slot-boundp obj 'value-object)
                               (oref obj value-object))))
      (propertize
       (format "(%s)" (ts--layout-child-desc value-object))
       'face 'transient-value)
    (propertize "¯\_(ツ)_/¯" 'face 'transient-inactive-value)))

;; Now that we have our class defined, we can create an infix the usual
;; way, just specifying our class
(transient-define-infix ts--inception-child-infix ()
  :class ts-child-infix)

;; All set!  This transient just tests our or new toy.
(transient-define-prefix ts-inception ()
  "Prefix that picks a suffix from its own layout."

  [["Pick a suffix"
    ("-s" "just a switch" "--switch") ; makes history value structure apparent
    ("c" "child" ts--inception-child-infix :class ts-child-infix)]

   ["Some suffixes"
    ("s" "wave surely" ts--wave-surely)
    ("d" "wave definitely" ts--wave-definitely)
    ("e" "wave eventually" ts--wave-eventually)
    ("C" "call & exit normally" ts--wave-normally :transient nil)]

   ["Read variables"
    ("r" "read args" ts-suffix-print-args )]])

;; (ts-inception)
;; Try setting the infix to "e" (yes, include quotes)
;; Try: (1 2)
;; Try: ts--wave-normally
;; Set the infix and re-open it
;; Save the infix, re-evaluate the prefix, and open the prefix again
;; Try flipping through history
;; Now do think of doing things like this with org ids, magit-sections, buffers etc.
(transient-define-suffix ts--inception-update-description ()
   "Update the description of of the selected child."
   (interactive)
   (let* ((args (transient-args transient-current-command))
          (description (transient-arg-value "--description=" args))
          ;; This is the part where we read the other infix
          (loc (car (cdr (assoc 'ts--inception-child-infix args))))
          (layout-child (transient-get-suffix 'ts-inception-update loc)))
     (cond
      ;; Once again, do different bodies based on what we found at the layout locition.
      ((or (listp layout-child) ; child
          (vectorp layout-child) ; group
          (stringp layout-child)) ; string child
       (if (stringp layout-child)
           (transient-replace-suffix 'ts-inception-update loc description) ; plain-text child
         (plist-put (elt layout-child 2) :description description)))
      (t (message (propertize (format
                               "Don't know how to modify whatever is at: %s"
                               loc) 'face 'warning))))
     ;; re-enter the transient manually to display the modified layout
     (transient-setup transient-current-command)))

(transient-define-prefix ts-inception-update ()
  "Prefix that picks and updates its own suffix."

  [["Pick a suffix"
    ("c" "child" ts--inception-child-infix)]

   ["Update the description!"
    ("-d" "description" "--description=") ; makes history value structure apparent
    ("u" "update" ts--inception-update-description :transient transient--do-exit)]

   ["Some suffixes"
    ("s" "wave surely" ts--wave-surely)
    ("d" "wave definitely" ts--wave-definitely)
    ("e" "wave eventually" ts--wave-eventually)
    ("C" "call & exit normally" ts--wave-normally :transient nil)]

   ["Read variables"
    ("r" "read args" ts-suffix-print-args )]])

;; (ts-inception-update)
;; Pick a suffix,
;; Then set the description
;; Then update the suffix's you picked with the new description!
;; Using a transient to modify a transient (⊃｡•́‿•̀｡)⊃━✿✿✿✿✿✿
;; Try to rename a group, such as (0 0)
;; Rename the very outer group, (0)
(transient-define-prefix ts-showcase ()
  "A launcher for a currated selection of examples.
While most of the prefixes have their :transient slot set to t, it's not
possible to return from all of them, especially if they demonstrate flow
control such as replacing or exiting."

  [["Layouts"
    ("ls" "stacked" ts-layout-stacked :transient t)
    ("lc" "columns" ts-layout-columns :transient t)
    ("lt" "stacked columns" ts-layout-stacked-columns :transient t)
    ("lg" "grid" ts-layout-the-grid :transient t)
    ("lp" "spaced out" ts-layout-spaced-out :transient t)
    ("le" "explicit class" ts-layout-explicit-classes :transient t)
    ("ld" "descriptions" ts-layout-descriptions :transient t)
    ("lD" "dynamic descriptions" ts-layout-dynamic-descriptions :transient t)]

   ["Nesting & Flow Control"
    ("fs" "stay transient" ts-stay-transient :transient t)
    ("fb" "binding sub-prefix" ts-simple-parent :transient t)
    ("fr" "sub-prefix with return" ts-simple-parent-with-return :transient t)
    ("fm" "manual setup in suffix" ts-parent-with-setup-suffix :transient t)
    ("fi" "mixing interactive" ts-interactive-basic :transient t)
    ("fe" "early completion" ts-simple-messager :transient t)]

   ["Managing State"
    ("sb" "a bunch of infixes" ts-basic-infixes :transient t)
    ("sc" "using scope (accepts prefix)" ts-scope :transient t)
    ("sn" "set & save / snowcones" ts-snowcone-eater :transient t)
    ("sp" "history key / ping-pong" ts-ping :transient t)
    ("sg" "always forget / goldfish" ts-goldfish :transient t)
    ("se" "always remember / elephant" ts-elephant :transient t)
    ("sd" "default values" ts-default-values :transient t)
    ("sf" "enforcing inputs" ts-enforcing-inputs :transient t)
    ("sl" "lisp variables" ts-lisp-variable :transient t)]

   ["CLI arguments"
    ("cb" "basic arguments" ts-switches-and-arguments :transient t)
    ("cm" "random-init infix" ts-maybe-on :transient t)
    ("cc" "basic choices" ts-animal-choices :transient t)
    ("ce" "exclusive switches" ts-exclusive-switches :transient t)
    ("ci" "incompatible switches" ts-incompatible :transient t)
    ("co" "completions for choices" ts-choices-with-completions :transient t)
    ("cc"  "cowsay cli wrapper" ts-cowsay :transient t)]

   ["Visibility"
    ("vp" "predicates" ts-visibility-predicates :transient t)
    ("vi" "inapt (not suitable)" ts-inapt-parent :transient t)
    ("vl" "levels" ts-levels-and-visibility :transient t)]

   ["Advanced"
    ("ac" "generated child" ts-generated-child :transient t)
    ("ag" "generated group" ts-generated-group :transient t)
    ("ai" "custom infixes" ts-inception :transient t)
    ("au" "custom infixes & update" ts-inception-update :transient t)]])

(provide 'transient-showcase)
;;; transient-showcase.el ends here
