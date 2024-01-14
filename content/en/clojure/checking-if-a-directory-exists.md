---
title:    "Clojure recipe: Checking if a directory exists"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why

As a Clojure programmer, you may come across the need to check if a directory exists. This can be for various reasons, such as verifying if a directory exists before creating a new one or ensuring that a specific path is valid before proceeding with file operations.

## How To

To check if a directory exists in Clojure, you can use the `clojure.java.io/file` function along with the `clojure.java.io/exists?` function. The `file` function takes in a string representing the directory path, while the `exists?` function returns a boolean indicating if the file exists or not.

```
Clojure
(def directory (file "path/to/directory"))
(println (exists? directory))
```

If the directory exists, the output will be `true`. However, if the directory does not exist, the output will be `false`.

## Deep Dive

When checking if a directory exists, it is important to understand how the `exists?` function works. This function uses the underlying `java.io.File` object to check if the file or directory exists. It will return `true` if the file or directory exists, or `false` if it does not exist or if there is an error in accessing the path.

Additionally, it is important to note that the `exists?` function only checks for the existence of the directory and not if it is a valid directory or if the user has permission to access it. These factors should be considered when using this function.

## See Also

For more information on checking file and directory existence in Clojure, you can refer to the following resources:

- [Clojure Documentation on File](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file)
- [Clojure Documentation on Java IO](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io.file)
- [Stack Overflow question on checking if a file exists](https://stackoverflow.com/questions/42434220/checking-if-a-file-exists-in-clojure)

Now that you know how to check if a directory exists in Clojure, you can use this knowledge in your future projects and ensure that your directory operations are error-free.