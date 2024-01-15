---
title:                "Creating a temporary file"
html_title:           "Clojure recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files in Clojure can be a handy tool for managing and accessing temporary data during your program's runtime. Whether you need to store temporary data for data processing or perform file operations, temporary files can provide a convenient and efficient solution.

## How To

To create a temporary file in Clojure, we can use the `with-open` macro in combination with the `io/file` function. This allows us to create a new temporary file and automatically close it after our code has executed.

```Clojure
(with-open [temp-file (io/file "temp.txt")]
  (println "Temporary file created!"))
```

In the above example, we have used the `println` function to show that the temporary file is successfully created. However, we can also perform various file operations on this temporary file, such as writing data to it or reading data from it.

To write data to the temporary file, we can use the `spit` function:

```Clojure
(with-open [temp-file (io/file "temp.txt")]
  (spit temp-file "Hello World!"))
```

Similarly, to read data from the temporary file, we can use the `slurp` function:

```Clojure
(with-open [temp-file (io/file "temp.txt")]
  (println (slurp temp-file)))
```

The `with-open` macro ensures that the temporary file is automatically closed after our code has executed, preventing any potential resource leaks.

## Deep Dive

When creating a temporary file, we can also specify the parent directory and file name. The `io/file` function accepts two parameters - the parent directory path and the file name, both of which are optional.

If no file name is provided, Clojure will generate a random name for the temporary file. This ensures that the temporary file has a unique name every time it is created.

We can also use the `io/file` function to specify the parent directory for our temporary file. This allows us to control where the temporary file is created and ensures that the file is deleted when our program exits, even if it throws an exception.

```Clojure
(let [temp-dir (io/file "temp-files")
      temp-file (io/file temp-dir "temp.txt")]
  (with-open [temp-file temp-file]
    (println "Temporary file created in temp-files directory!")))
```

## See Also

- [Clojure's io/file function documentation](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/io)
- [Official Clojure documentation on creating temporary files](https://clojure.org/reference/java_interop#with-open)
- [Clojure Cookbook's chapter on temporary files](https://cljs.github.io/api/io/create-file.html)