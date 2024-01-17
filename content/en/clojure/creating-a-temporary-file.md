---
title:                "Creating a temporary file"
html_title:           "Clojure recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Creating a temporary file is the process of generating a file that is used to store temporary data during the execution of a program. This is often done for various reasons such as caching data, temporary storage of files, or passing data between different processes or systems.

## How to:
Creating a temporary file in Clojure is a simple process using the `with-open` function and the `java.io.File` class. First, we import the necessary libraries:
```Clojure
(ns temp-file-demo.core
  (:import [java.io File]))
```
Then, we use the `with-open` function to create a temporary file and execute some code using that file:
```Clojure
(with-open [temp-file (File/createTempFile "temp-" ".txt")]
  (println "Temporary file created at: " (.getAbsolutePath temp-file)))
```
The output will be the path of the temporary file created, typically something like `/tmp/temp-837498379.txt`.

## Deep Dive:
The ability to create temporary files is not unique to Clojure and has been around since the early days of programming. However, modern programming languages, including Clojure, provide built-in functions and libraries to easily create and manage temporary files.

Alternative methods of creating temporary files include using the `java.nio.file.Files` library or using third-party libraries like `org.apache.commons.io.FileUtils`.

Deeper into the implementation, creating a temporary file involves generating a unique name, creating the file, and marking it to be deleted on program termination. This ensures that the temporary file is only used for its intended purpose and does not clutter the file system.

## See Also:
- [Official Java documentation on creating temporary files](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String,%20java.io.File))
- [ClojureDocs on the with-open function](https://clojuredocs.org/clojure.core/with-open)
- [Alternative method using java.nio.file.Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html#temporary)
- [Apache Commons IO library](https://commons.apache.org/proper/commons-io/)