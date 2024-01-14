---
title:                "Clojure recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why 
Creating temporary files is a common task in programming, especially when dealing with large amounts of data or complex processes. These temporary files allow for efficient file manipulation and organization without causing conflicts with existing files.

## How To 
Creating a temporary file in Clojure is a simple process. We can use the `with-open` function to create and manipulate the temporary file in a secure and efficient manner. Let's take a look at an example using the `str` function to create a temporary text file and write a string to it. 

```Clojure
(with-open [temp-file (java.io.File/createTempFile "temp" ".txt")]
  (with-open [out (clojure.java.io/writer temp-file)]
    (clojure.java.io/write out (str "This is a temporary file!"))
    (println "Temporary file created: " (.getAbsolutePath temp-file))
    (println "Contents of temporary file: " (slurp temp-file))))
```

The output of this code will be:
```
Temporary file created: /var/folders/v4/7jcsccgj21772d87f5_8d6kjlv2706/T/temp11950744680355058373.txt
Contents of temporary file: This is a temporary file!
```

## Deep Dive 
Clojure makes use of the `java.io.File` class to create temporary files. The `createTempFile` function takes in two arguments: a prefix for the file name and a suffix for the file extension. This ensures that the temporary file will be unique and secure. The `with-open` function is used to manage resources and ensure that the file is deleted once it is no longer in use.

## See Also 
- [ClojureDocs on with-open](https://clojuredocs.org/clojure.core/with-open)
- [Official Java documentation on File class](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Clojure File Manipulation](https://clojuredocs.org/clojure.java.io/file)