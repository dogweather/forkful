---
title:    "Clojure recipe: Creating a temporary file"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Why

Creating temporary files is a common practice in programming and can serve various purposes, such as storing temporary data, creating backups, or facilitating communication between different processes. In this blog post, we will explore how to create temporary files in Clojure and some use cases for doing so.

## How To

In Clojure, we can use the `with-open` macro to create and manage temporary files. This macro allows us to open a file or stream, execute some code, and then automatically close the file or stream. This is particularly useful for temporary files, as it ensures that the file will be deleted once it is no longer needed.

Let's see an example of how to create a temporary file using the `with-open` macro:

```Clojure
(with-open [temp-file (java.io.File/createTempFile "temp" ".txt")]
  (println "Temporary file created at:" (.getPath temp-file)))
```

In this code, we are using the Java `File` class to create a temporary file with the prefix "temp" and the suffix ".txt". We are then printing out the path to the created file using the `getPath` method.

Running this code will output something like this:

```
Temporary file created at: /var/folders/gr/3bdslrps3m50w19rng8ty89c0000gn/T/temp7109007803671371312.txt
```

As you can see, a temporary file with a unique name and location is created and automatically deleted once the code execution is finished. 

## Deep Dive

Under the hood, the `java.io.File/createTempFile` method uses the system's default temporary directory to create the file. However, we can also specify a specific directory where we want the temporary file to be created by passing a `java.io.File` object as the third argument.

Apart from creating temporary files, the `with-open` macro can also be used to create temporary readers, writers, and input/output streams. Here's an example of creating a temporary reader and reading data from it:

```Clojure
(with-open [temp-reader (java.io.StringReader. "Hello World")]
  (println "Data read from temporary reader:" (.readLine temp-reader)))
```

This code will print "Hello World" as the data read from the temporary reader.

## See Also

- Official Clojure documentation for `with-open`: https://clojuredocs.org/clojure.core/with-open
- Java `File` class documentation: https://docs.oracle.com/javase/7/docs/api/java/io/File.html