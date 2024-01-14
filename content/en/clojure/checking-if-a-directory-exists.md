---
title:                "Clojure recipe: Checking if a directory exists"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

As a programmer, there may be several reasons why you would need to check if a directory exists. One common scenario is when your code needs to access a specific directory in order to read or write files. In order to avoid errors and handle potential issues, it is important to first verify if the directory exists before proceeding with your code.

## How To

Checking if a directory exists in Clojure is a simple task. You can use the `exists?` function from the `java.io.File` namespace to check if a directory exists at a given path.

```
Clojure
(require '[clojure.java.io :as io])

(def directory "/path/to/directory")

(io/exists? directory)
```

The `exists?` function will return `true` if the directory exists and `false` if it does not.

```
true
```

You can also use the `exists?` function to check for subdirectories within a given directory path.

```
Clojure
(io/exists? "/path/to/directory/subdirectory")
```

If the subdirectory exists, it will return `true`. Otherwise, it will return `false`.

## Deep Dive

Under the hood, the `exists?` function checks if the given path is a valid file or directory using the `java.io.File` class. If it exists, it will return `true`; otherwise, it will return `false`. 

It is important to note that the `exists?` function will only check for the existence of the file or directory at the specific path given. It does not guarantee that the file or directory is accessible or that you have permission to read or write to it. For more advanced file operations, you can use Clojure's `clojure.java.io` library to check for file permissions and handle file operations.

## See Also

- Official Clojure `java.io.File` documentation: https://clojuredocs.org/clojure.java.io/file
- Clojure `clojure.java.io` library: https://clojuredocs.org/clojure.java.io