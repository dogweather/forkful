---
title:                "Checking if a directory exists"
html_title:           "Clojure recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why

If you are a Clojure developer, you may encounter situations where you need to check if a directory exists before performing certain operations. This can be useful for error handling or ensuring that your code runs smoothly without any unexpected errors.

## How To

To check if a directory exists in Clojure, we can use the `exists?` function from the `clojure.java.io` namespace. This function takes in a path as a string and returns a boolean value indicating if the directory exists or not.

```
Clojure (exists? "path/to/directory")
=> true
```

If the directory does not exist, the function will return false. We can also use this function to check for the existence of a file by passing in the file path instead.

```
Clojure (exists? "path/to/file.txt")
=> false
```

In addition, we can use the `file-seq` function to get a sequence of files and directories inside a given directory. This function returns a lazy sequence of `File` objects which we can use with the `exists?` function to check for their existence.

```
Clojure (exists? (first (file-seq "path/to/directory")))
=> true
```

## Deep Dive

Under the hood, the `exists?` function uses Java's `File` class to check for the existence of the given path. This means that the function will return true for both directories and files, as `File` objects represent both.

It is worth noting that the `exists?` function does not check for the validity or accessibility of a given path, it only checks for its existence. If you need to ensure that a directory is both valid and exists, you can use the `file` function to create a `File` object and then use the `exists?` function on it.

In addition, the `clojure.java.io` namespace also offers other useful functions such as `delete-directory` for deleting a directory, `file-seq` for listing files and directories, and `make-directory` for creating a new directory.

## See Also

- [Clojure Java Interop guide](https://clojure.org/reference/java_interop)
- [Clojure API reference for the clojure.java.io namespace](https://clojuredocs.org/clojure.java.io)

By using the `exists?` function, we can easily handle cases where a directory may not exist, ensuring a smoother and more reliable code execution. Make sure to also check out the other useful functions available in the `clojure.java.io` namespace for more file and directory handling capabilities.