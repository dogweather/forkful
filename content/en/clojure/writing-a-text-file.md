---
title:                "Writing a text file"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"

category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file involves creating or altering text data and saving it to a file on your storage medium. Programmers do it for data logging, configuration settings, or exporting human-readable reports.

## How to:

In Clojure, you use the `spit` function to write data to a text file. It's straightforward:

```clojure
(spit "example.txt" "Hello, World! This is Clojure speaking.")
```

The `spit` function takes the filename and the content. To append content, set the `append` flag:

```clojure
(spit "example.txt" "\nLet's add this new line." :append true)
```

Sample output for `example.txt` after both operations:

```
Hello, World! This is Clojure speaking.
Let's add this new line.
```

## Deep Dive

Clojure’s `spit` function comes from its "I/O" library - a successor to Lisp's legacy of concise file operations. Alternatives in Clojure include `clojure.java.io/writer` for buffered writing and libraries like `slurp` for reading files. When using `spit`, remember it's not meant for large streams of data due to potential memory issues - use `writer` and loop over the data instead.

## See Also

- Clojure Docs for `spit`: [https://clojuredocs.org/clojure.core/spit](https://clojuredocs.org/clojure.core/spit)
- Clojure `java.io` wrapper: [https://clojure.github.io/clojure/clojure.java.io-api.html](https://clojure.github.io/clojure/clojure.java.io-api.html)
