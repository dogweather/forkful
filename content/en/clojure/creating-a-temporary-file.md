---
title:                "Clojure recipe: Creating a temporary file"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files is a common practice in programming, especially in Clojure. These files serve as a placeholder for data that is only needed temporarily during the execution of a program. They allow for efficient and organized data management, improving the performance and functionality of a program.

## How To

Creating a temporary file in Clojure is a simple process that can be done using the `java.io.File` class. The following code block shows an example of how to create a temporary file in Clojure and print its name:

```Clojure
(let [temp-file (java.io.File/createTempFile "clojure-temp" ".txt")]
  (println (.getName temp-file))) ; outputs "clojure-temp2047400008450058639.txt"
```

In this code, we use the `createTempFile` method to create a new temporary file with a prefix of "clojure-temp" and a suffix of ".txt". The file's name will be a combination of the prefix, a random number, and the suffix. We then use the `getName` method to retrieve the name of the temporary file and print it to the console.

## Deep Dive

When creating a temporary file, we can also specify a directory where the file should be created. By default, the file will be created in the default temporary directory of the system, but we can choose to create it in a specific directory by passing a file object as the second argument to the `createTempFile` method.

Another important aspect to consider when creating temporary files is their lifespan. Temporary files are automatically deleted when the program terminates, but we can also delete them manually using the `deleteOnExit` method. This method takes no arguments and will mark the file for deletion when the program exits.

## See Also

- [Clojure Java Interop Guide](https://clojure.org/reference/java_interop)
- [Official Java Documentation on temporary files](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html#createTempFile(java.lang.String,java.lang.String,java.io.File))
- [Clojure File API Docs](https://clojuredocs.org/clojure.java.io/file)