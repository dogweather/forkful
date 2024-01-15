---
title:                "Writing a text file"
html_title:           "Clojure recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file is a common task in programming that allows you to store and manipulate data. Whether it's for creating reports, managing configurations, or storing user input, being able to write to a text file is a crucial skill for any programmer.

## How To

To write a text file in Clojure, you will need to use the `clojure.java.io` namespace, which provides functions for working with files and streams. Let's take a look at how to create a new text file and write data to it.

```
Clojure (require '[clojure.java.io :as io])

(def output-file "data.txt")

(with-open [out-file (io/writer output-file)] ; open a writer for the file
  (.write out-file "Hello world!") ; write data to the file
)
```

Running this code will create a new file named `data.txt` and write the string "Hello world!" to it. 

You can also use the `with-open` macro to ensure that the file is automatically closed after the code is executed, even in case of exceptions.

```
(with-open [out-file (io/writer output-file)]
  (doseq [line ["This is a sentence" "This is another sentence"]] ; use doseq to write multiple lines
    (.write out-file line)
    (.write out-file \newline) ; add a new line character 
  )
)
```

The `with-open` macro works with any object that implements the `Closeable` interface, so you can use it to manage reading and writing to files, as well as other types of IO operations.

## Deep Dive

Clojure also includes the `slurp` and `spit` functions for working with files as strings. These functions handle opening and closing the file automatically, so you don't have to worry about managing streams.

```
(spit output-file "This will overwrite the existing file content") ; create and write to a file
(slurp output-file) ; read the entire file as a string
```

You can also specify encoding options and append to existing files using these functions. Additionally, Clojure offers a `io/copy` function for copying from one input stream to an output stream, which can be useful for creating backups or transferring data between files.

## See Also

- [Official Clojure documentation on file I/O](https://clojure.org/reference/java_interop#_working_with_files)
- [ClojureDocs: clojure.java.io](https://clojuredocs.org/clojure.java.io)
- [Learn Clojure in Y minutes](https://learnxinyminutes.com/docs/clojure/)