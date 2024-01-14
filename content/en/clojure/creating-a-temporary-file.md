---
title:    "Clojure recipe: Creating a temporary file"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files is a common task in programming, especially when working with large or complex data. Temporary files provide a convenient way to store data that is only needed temporarily, without cluttering up the main file system.

## How To

To create a temporary file in Clojure, we can use the `java.io.File` class along with the `with-open` macro. First, we need to import the `java.io` package:

```Clojure
(import '[java.io File])
```

Then, we can use the `with-open` macro to create a temporary file and write data to it:

```Clojure
(with-open [temp-file (File/createTempFile "mytempfile" ".txt")]
  (println "Writing data to temporary file...")
  (spit temp-file "This is temporary data.")
  (println "Temporary file created at" (.getAbsolutePath temp-file)))
```

Running this code will result in the temporary file "mytempfile.txt" being created in the default temporary file directory, with the contents "This is temporary data." The `with-open` macro will also automatically close the file after the code block has executed.

## Deep Dive

The `createTempFile` method in the `java.io.File` class is used to create a temporary file with a specified prefix and suffix. The prefix is the first parameter, and the suffix is the second parameter. For example, in our code block, we specified the prefix as "mytempfile" and the suffix as ".txt", resulting in a temporary file named "mytempfile.txt". If no prefix is specified, a default prefix of "tmp" will be used.

Additionally, the `with-open` macro is a convenient way to open and close files in Clojure. It takes a series of bindings as parameters, where each binding pairs a symbol with an open resource. In our code block, we used a single binding for the `temp-file` symbol and the `File/createTempFile` expression. This allows us to work with the temporary file inside the `with-open` code block, and the file will be closed automatically once the code block has finished executing.

## See Also

- [Java File Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Official Clojure Documentation on IO](https://clojure.org/reference/io)