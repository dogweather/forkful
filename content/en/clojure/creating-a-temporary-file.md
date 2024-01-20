---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Unwrapping Temporary File Creation in Clojure

## What & Why?
Creating a temporary file is a typical task in programming performed to temporarily store data, often because it's too large to hold in memory. It supports operations such as sorting large data sets or storing user session data in web applications.

## How to:
To create a temporary file in Clojure, you can use Java interop. Clojure, being a hosted language on JVM, allows this. Here's a simple example:

```Clojure
(import 'java.nio.file.Files)
(import 'java.nio.file.Paths)

(defn create-temp-file []
  (.toString (Files/createTempFile (Paths/get "./" nil) "temp" ".txt")))
```
Call the function to create a temporary file in the current directory:

```Clojure
(create-temp-file)
;=> "./temp7205760672390790.txt"
```

Here, "temp" is the prefix and ".txt" is the suffix of the temporary file. 

## Deep Dive
Clojure, launched in 2007, did not initially have clear, in-built capabilities for file I/O operations that later languages adopted. The developers frequently fell back to using Java interop for tasks like creating temporary files. 

An alternative to using Java Interop is using libraries such as `clojure.java.io`. While Java Interop prevents unnecessary abstraction, additional libraries can provide a more idiomatic Clojure feel. 

When `Files/createTempFile` is invoked, Java's File I/O API creates a new, empty file in the specified directory. This file is temporary, meaning it will automatically be deleted when the JVM is terminated, thus preventing unwanted accumulation of such files. This detail is handy when you need temporary storage, but don't want to manage the cleanup.

## See Also
For further reading, refer to:

1. [Clojure java.io Documentation](https://clojure.github.io/clojure/clojure.java.io-api.html)
2. [Java nio Files Documentation](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
3. [Working with Files in Clojure](https://www.baeldung.com/clojure-file-io)

Remember, practice is a key part of wrapping your head around this, so try it out!