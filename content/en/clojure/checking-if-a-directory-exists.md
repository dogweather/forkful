---
title:    "Clojure recipe: Checking if a directory exists"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Why
As a programmer, you may encounter situations where you need to check if a directory exists. This can be for several reasons such as verifying if a file can be saved to a specific location or ensuring that a file can be accessed before performing operations on it.

## How To
Checking if a directory exists in Clojure is a simple process that can be accomplished using the `.isDirectory()` method from the `java.io.File` class. Here's an example of how you can do this:

```Clojure
(import '(java.io File))

(def directory (File. "/my/directory/path"))

(println (.isDirectory directory))
```

This code first imports the `File` class from the `java.io` package. Then, we define a variable `directory` to store the path of the directory we want to check. Finally, we use the `isDirectory` method to determine if the directory exists or not, and print the output.

If the directory exists, the output will be `true`, and if it doesn't exist, the output will be `false`.

## Deep Dive
Behind the scenes, the `isDirectory` method in Clojure checks the file path and verifies if it exists and if it is indeed a directory. If the path does not exist or if it is a file instead of a directory, the method will return `false`.

It's worth noting that this method only checks for the existence of the directory, it does not verify if you have access to it or not. Additionally, `isDirectory` can only check for directories within the local file system, not remote directories.

## See Also
- Official Clojure Documentation: https://clojuredocs.org/clojure.java.io/file.cp
- Baeldung tutorial on checking if a file/directory exists: https://www.baeldung.com/java-check-directory-exists