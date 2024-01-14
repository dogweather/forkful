---
title:                "Clojure recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to check if a directory exists in your Clojure program? Knowing how to do so can save you time and prevent unexpected errors in your code.

## How To

To check if a directory exists in Clojure, you can use the ```(file-exists? path)``` function from the ```clojure.java.io``` namespace. This function takes in a string representing the directory path and returns ```true``` if the directory exists and ```false``` if it does not.

```Clojure
(require '[clojure.java.io :as io])

(io/file-exists? "path/to/directory") ; returns true if directory exists
(io/file-exists? "path/to/nonexistent/directory") ; returns false
```

You can also use the ```(dir? f)``` function to specifically check if a path points to a directory. This function takes in a Java ```File``` object as an argument and returns ```true``` if the file is a directory and ```false``` if it is not.

```Clojure
(require '[clojure.java.io :as io])

(io/dir? (io/file "path/to/directory")) ; returns true if directory exists
(io/dir? (io/file "path/to/file")) ; returns false
```

## Deep Dive

Under the hood, the ```(file-exists? path)``` function calls the Java ```File.exists()``` method to check if the directory exists. The ```(dir? f)``` function uses the ```File.isDirectory()``` method to determine if the file is a directory.

It is important to note that these functions only check if the directory exists in the filesystem, not if the current user has permissions to access it. 

## See Also

- [Clojure.io documentation](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file-exists?)
- [Java File documentation](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)