---
What and Why: "Debuggers help you understand your program's \"flow\"; making sure\
  \ its logic plays out as expected. For example, you can see the values of your variables\
  \ at any point in the code. \n\nI find them useful for the most difficult bugs.\
  \ IMO, their helpfulness really depends on the particular language, your personal\
  \ working style, the quality of the debugging tool itself. \u2014Robert"
date: 2024-01-25 20:50:12.035385-07:00
description: "How to: Clojure leans on the Java Virtual Machine (JVM), so a lot of\
  \ debugging happens with Java tools. One such tool is `CIDER`, a powerhouse package\
  \ for\u2026"
lastmod: 2024-03-31
model: gpt-4-1106-preview
summary: Clojure leans on the Java Virtual Machine (JVM), so a lot of debugging happens
  with Java tools.
title: Using a debugger
weight: 35
---

## How to:
Clojure leans on the Java Virtual Machine (JVM), so a lot of debugging happens with Java tools. One such tool is `CIDER`, a powerhouse package for Clojure development in Emacs, which has solid debugging capabilities. Let's dive in:

```clojure
;; First, jack-in to a Clojure project within Emacs using CIDER
M-x cider-jack-in

;; Set a breakpoint
;; Navigate to the line in your Clojure code you want to inspect and
;; press "C-c M-b" or execute:
M-x cider-debug-defun-at-point

;; When the code runs, you'll hit the breakpoint. CIDER will prompt you with:
;; 1. n to go to the next logical step in execution,
;; 2. c to continue execution until the next breakpoint,
;; 3. q to quit debugging.

;; Inspect locals at breakpoint
;; While at a breakpoint, type:
locals

;; You'll see a list of local variables and their values printed in the minibuffer.
```
Sample output may look like:
```clojure
{:x 10, :y 20, :result 200}
```

## Deep Dive
The debugger is a tool as old as the hills in computing terms. The term "bug" was coined back in the early days of computing when an actual insect caused an error by shorting a circuit in a machine.

While `CIDER` is great for Emacs enthusiasts, there are alternatives for Clojure debugging. For instance, using IntelliJ with the Cursive plugin can give a more GUI-driven debugging experience. Plus, you can use the in-built Leiningen or tools.deps to control the process flow when debugging.

Under the hood, these debuggers often manipulate bytecodes, perform evaluations in dedicated nREPL sessions, and offer stack trace inspection. They're leveraging the underlying JVM's capabilities, tapping into the wealth of Java’s debugging frameworks.

## See Also
- [CIDER Debugger Documentation](https://docs.cider.mx/cider/debugging/debugger.html)
- [Cursive Debugger](https://cursive-ide.com/userguide/debugging.html)
- [Leiningen for Automation and Debugging](https://leiningen.org/)
- [tools.deps.alpha for more control](https://github.com/clojure/tools.deps.alpha)
