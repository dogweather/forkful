---
title:                "Reading a text file"
date:                  2024-01-20T17:53:51.423033-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading a text file"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file means getting data from a file stored on your disk into your program. Programmers do this to process or analyze content without manual input, automate tasks, or parse configuration data.

## How to:

```Clojure
;; Read entire file as string
(slurp "example.txt")

;; Output: "Hello, this is your file content!"

;; Read file line-by-line
(with-open [reader (clojure.java.io/reader "example.txt")]
  (doseq [line (line-seq reader)]
    (println line)))

;; Output:
;; Hello,
;; this is your
;; file content!
```

## Deep Dive

Traditionally, reading files in programming languages was a verbose task with many steps to handle errors and resources. In Clojure, you benefit from the `slurp` function, an elegant one-liner to grab the whole file's content. For line-by-line reading, `line-seq` coupled with `with-open` ensures efficient and safe file handling. It's also worth mentioning that while `slurp` is handy, it's not ideal for large files due to memory constraints. That's when `line-seq` shines, as it reads the file lazily, one line at a time.

Alternatives for reading files in Clojure include using the `clojure.java.io/file` with functions like `reader` and constructs like `with-open` to manage the file handle manually. The trade-off here is between ease of use (`slurp`) and fine-grained control combined with resource safety (`with-open` and `reader`).

Implementation-wise, Clojure's approach is grounded in Java's IO classes, therefore when you're dealing with files in Clojure, you're dealing with Java's mature, well-tested libraries, wrapped in a functional idiom. Always keep an eye on resources: open files consume handles and memory, so clean file handling is a neat habit.

## See Also

- ClojureDocs for `slurp`: https://clojuredocs.org/clojure.core/slurp
- ClojureDocs for `line-seq`: https://clojuredocs.org/clojure.core/line-seq
- Java interop in Clojure: https://clojure.org/reference/java_interop
- Working with files in Clojure (Practical.li): https://practical.li/clojure/working-with-files.html
