---
title:                "Reading a text file"
html_title:           "Clojure recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

If you've ever needed to process or extract information from a text file, you know how tedious and time-consuming it can be to manually search and edit the file. This is where Clojure comes in, as it offers efficient and elegant tools for reading and manipulating text files.

## How To

To read a text file in Clojure, we will use the `slurp` function. This function takes in a path to a text file and returns the contents of the file as a string.

```Clojure
(def file-content (slurp "my-file.txt"))
```

Now that we have the contents of the file, we can use various functions and operations to manipulate the data. For example, we can split the string into a sequence of lines using the `clojure.string/split-lines` function.

```Clojure
(def lines (clojure.string/split-lines file-content))
```

We can also use the `clojure.string/split` function to split the string into a sequence of words, using a specified delimiter.

```Clojure
(def words (clojure.string/split file-content #"\s+"))
```

Once we have the data in a structured format, we can use functions like `filter` or `map` to extract specific information or transform the data. For instance, we can filter out all the lines that contain a specific word using the `filter` function.

```Clojure
(def filtered-lines (filter #(.contains % "Clojure") lines))
```

## Deep Dive

One important thing to note when reading text files in Clojure is that the `slurp` function reads the entire contents of the file into memory as a string. This means that if the file is large, we might run into memory issues.

To avoid this, we can use the `with-open` macro, which will automatically close the file after reading it, thus freeing up the memory resources.

```Clojure
(with-open [rdr (clojure.java.io/reader "my-file.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))
```

Additionally, Clojure also offers the `clojure.java.io/reader` function, which allows for more advanced options when reading a file, such as specifying character encoding.

## See Also

- [Clojure Official Documentation on Reading Files](https://clojure.org/reference/io)
- [ClojureDocs page on Slurp function](https://clojuredocs.org/clojure.core/slurp)
- [Blog post on working with large text files in Clojure](https://indielambda.com/blog/working-with-large-text-files-in-clojure/)