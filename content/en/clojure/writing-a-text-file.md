---
title:    "Clojure recipe: Writing a text file"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why

Writing a text file is an essential aspect of programming. It allows us to persist data and communicate information in a human-readable format. In this blog post, we will explore the process of writing a text file in Clojure and its importance in creating robust and manageable code.

## How To

Writing a file in Clojure is a simple and straightforward process. We will start by requiring the `clojure.java.io` namespace, which provides functions for working with the file system.

```Clojure
(require '[clojure.java.io :as io])
```

Next, we will create a file object using the `io/file` function and specify the path and name of the file we want to create.

```Clojure
(def file (io/file "sample.txt"))
```

We can then write to the file using the `spit` function. This function takes in the file object and the data we want to write to the file.

```Clojure
(spit file "Hello World!")
```

To write multiple lines to the file, we can use the `with-open` macro, which ensures that the file is properly closed after writing.

```Clojure
(with-open [f (io/writer file)]
  (.write f "This is the first line")
  (.write f "This is the second line")
  (.write f "This is the third line"))
```

Finally, we can use the `slurp` function to read the data from the file back into our program.

```Clojure
(slurp file) ;; => "This is the first line\nThis is the second line\nThis is the third line"
```

## Deep Dive

Behind the scenes, when we write to a file using `spit` or `with-open`, Clojure uses Java's `FileOutputStream` and `BufferedWriter` classes to handle the I/O operations. It is important to note that unlike other languages, in Clojure, writing a file is an atomic operation. This means that the entire file is written at once, rather than line by line. This ensures consistency and helps avoid data corruption in case of any interruptions.

We can also use the `append` function if we want to add data to an existing file, or specify the character encoding using the `:encoding` option. Additionally, the `file-seq` function can be used to traverse a directory and find all the files within it.

## See Also

- [Official Clojure Documentation on File Operations](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Java File I/O Documentation](https://docs.oracle.com/javase/tutorial/essential/io/fileio.html)
- [Practical Common Lisp Chapter on Writing Files](http://www.gigamonkeys.com/book/files.html)