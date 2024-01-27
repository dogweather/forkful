---
title:                "Checking if a directory exists"
date:                  2024-01-19
html_title:           "C recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists means confirming whether a path points to a directory on your file system. Programmers do it to prevent errors, ensure correct file handling, and setup necessary conditions before performing file operations.

## How to:
Use `clojure.java.io/file` to create a File object and `.exists` to check if it exists. The `isDirectory` method confirms if the File is a directory.

```Clojure
(require '[clojure.java.io :as io])

(defn directory-exists? [path]
  (let [dir (io/file path)]
    (and (.exists dir) (.isDirectory dir))))

;; Example usage:
(directory-exists? "/path/to/directory") ;=> true or false
```
Sample Output:
```
true ; if the directory exists
false ; if the directory does not exist
```

## Deep Dive
Historically, a similar process is used in Java; since Clojure runs on the JVM, it leverages Java libraries for file system operations. Alternatives in Clojure could involve using other Java functions or libraries like `nio.file.Files`. Under the hood, checking for a directory's existence can be IO intensive and may behave differently across operating systems and file system permissions, which is why ensuring its existence before performing further operations is crucial.

## See Also
- Clojure Docs on I/O: [https://clojure.github.io/clojure/clojure.java.io-api.html](https://clojure.github.io/clojure/clojure.java.io-api.html)
- Java's File Class: [https://docs.oracle.com/javase/8/docs/api/java/io/File.html](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- Java's NIO Files Class: [https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
