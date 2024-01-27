---
title:                "Writing to standard error"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to standard error (`stderr`) is a way to output error messages and diagnostics. Programmers do it to separate these from regular output (`stdout`), which makes debugging and logging easier.

## How to:
To write to standard error in Clojure, you'd use `binding` with `*err*`. Here's a quick example:

```Clojure
(binding [*err* *out*]
  (println "This will go to standard error"))
```

Sample output (in your shell):

```
$ clj your_script.clj 2> error.log
$ cat error.log
This will go to standard error
```

This snippet binds `*err*` to `*out*`, which is standard output, so you can see what would typically go to `stderr`.

## Deep Dive
Historically, Unix systems featured two separate output streams, `stdout` and `stderr`, for different data types. In Clojure, `*out*` refers to `stdout` and `*err*` to `stderr`. Alternatives to `binding` include using Java interop directly (e.g., `(.println System/err "message")`). Implementation-wise, `*err*` is a dynamic var, allowing for thread-local bindings — a nuance that can affect how errors are logged in concurrent applications.

## See Also
- Clojure Docs on `*err*`: https://clojuredocs.org/clojure.core/*err*
- Clojure Docs on `binding`: https://clojuredocs.org/clojure.core/binding
- Java API for `PrintStream` (which `System/err` is): https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html

For a broader understanding of standard streams, the following can also be useful:
- Wikipedia on Standard Streams: https://en.wikipedia.org/wiki/Standard_streams
- The Unix standard streams documentation: `man stdio`
