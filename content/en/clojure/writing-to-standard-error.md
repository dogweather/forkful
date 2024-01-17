---
title:                "Writing to standard error"
html_title:           "Clojure recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to standard error, also known as stderr, is a way for programmers to output error messages or log information separate from regular program output, which is usually sent to standard output or stdout. By writing to stderr, programmers can differentiate between normal output and error messages, making it easier to identify and troubleshoot issues in their code.

## How to:
```Clojure
;; To write to stderr, you can use the `pr` function.
(pr "This is an error message") ;; Outputs "This is an error message" to stderr

(prn "This is another error message") ;; Outputs "This is another error message" to stderr with a newline

;; You can also specify the output stream explicitly by using the `pst` function.
(pst err "Error message with explicit output stream") ;; Outputs "Error message with explicit output stream" to stderr

;; To redirect stdout to stderr, you can use `set!` on the `*out*` global variable.
(set! *out* *err*) ;; From this point on, all output sent to stdout will be redirected to stderr

;; To revert back to the original stdout, simply set *out* to the original value.
(set! *out* (io/writer System/out)) ;; Sets *out* to the original value of System/out

;; Finally, you can also use the `eprintln` function to write error messages directly to stderr.
(eprintln "Another error message") ;; Outputs "Another error message" to stderr

```

## Deep Dive:
Writing to stderr has been a standard practice in programming for a long time, with its origins dating back to the early Unix systems. Before the introduction of standard streams, error messages and regular output were all mixed together, making it difficult to distinguish between the two. By separating them, programmers could easily identify and troubleshoot errors.

An alternative to writing to stderr is using logging frameworks or libraries, which provide more advanced functionality such as formatting and filtering of log messages. However, writing to stderr is still a useful practice, especially in scenarios where setting up a logging framework may not be feasible, such as in small scripts or command-line tools.

In Clojure, writing to stderr is achieved by using the `pr` and `prn` functions, which are part of the Clojure's core library. By default, `pr` outputs to *out*, while `prn` outputs to *out* followed by a newline character. To output to stderr explicitly, you can use the `pst` function.

## See Also:
- [https://clojure.org/reference/io] for more information on Clojure's IO functions
- [https://en.wikipedia.org/wiki/Standard_streams] for a deeper dive into standard streams in programming
- [https://stackoverflow.com/questions/5828872/why-do-we-output-error-to-stderr] for a discussion on why programmers output errors to stderr.