---
title:                "Writing a text file"
html_title:           "Clojure recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Writing a text file is essentially creating a document in plain text format, with no formatting or special characters. Programmers often use text files to store data in a human-readable format, such as configurations, logs, or even code itself. It's an easy and efficient way to store information that can be easily accessed and edited by both humans and machines.

## How to:

To create a text file in Clojure, we can use the `spit` function. It takes two arguments - the filename and the content of the file, and creates a new text file in the specified location with the given content.

```Clojure
(spit "mytextfile.txt" "Hello world!")
```
In this example, we are creating a text file called `mytextfile.txt` with the content "Hello world!".

To append content to an existing text file, we can use the `spit` function with the `:append` keyword.

```Clojure
(spit "mytextfile.txt" "Hello again!" :append)
```
This will add the string "Hello again!" to the end of the existing text file.

## Deep Dive:

The ability to read and write text files has been a fundamental feature of programming languages since their inception. Before the widespread use of databases and cloud storage, text files were the go-to method for storing and accessing data. Even today, text files are still widely used for simplicity and compatibility with other systems.

While Clojure provides functions for reading and writing text files, there are also other options such as using Java interop or using libraries like `clojure.java.io` or `clojure.data.json`. These alternatives may offer more features or customization options.

Internally, Clojure uses Java's `java.io.FileWriter` class to write text files. The `spit` function is just a convenience wrapper for this class, making it easier to write text files in Clojure.

## See Also:

- [The Clojure Documentation on `spit`](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/spit)
- [Java's `java.io.FileWriter` class documentation](https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html)