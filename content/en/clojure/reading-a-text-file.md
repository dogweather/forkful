---
title:                "Clojure recipe: Reading a text file"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Reading text files is a common task in programming, especially in data analysis and processing tasks. Understanding how to read and manipulate text files allows for greater control and flexibility in working with data.

## How To

To read a text file in Clojure, we can use the `slurp` function. This function takes in the file path as an argument and returns the contents of the file as a string. Let's see an example:

```Clojure
(def file (slurp "example.txt"))
(println file)
```
This code will print the contents of the "example.txt" file to the console. We can also use the `clojure.java.io` library to read a text file line by line using the `reader` function. Let's see an example:

```Clojure
(require '[clojure.java.io :as io])

(with-open [rdr (io/reader "example.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))
```

This code opens the "example.txt" file, reads it line by line, and prints each line to the console. 

## Deep Dive

When using the `slurp` function, we can also specify an encoding type if the text file is not in the default UTF-8 format. For example, if we have a file in ISO-8859-1 encoding, we can read it like this:

```Clojure
(def file (slurp "example.txt" :encoding "ISO-8859-1"))
(println file)
```

It's also important to note that the `slurp` function reads the entire file into memory, so it might not be suitable for large files. In those cases, using the `reader` function and reading the file line by line would be a better option.

## See Also

- Clojure.io Library: https://clojure.github.io/clojure/clojure.java.io-api.html
- Clojure String Functions: https://clojuredocs.org/clojure.core#functions-for-manipulating-strings
- Blog post on Reading and Writing files in Clojure: http://bendyworks.com/blog/article/clojure-reading-writing-files