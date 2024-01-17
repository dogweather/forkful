---
title:                "Reading a text file"
html_title:           "Clojure recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Reading a text file is the ability to access and extract data from a plain text document. This is a common practice for programmers as text files are often used to store information that can be easily manipulated and processed using code.

## How to:
In Clojure, reading a text file is a simple process that can be done using the `slurp` function. This function takes in a file path as its argument and returns the contents of the file as a string. Here's an example code block showing how to use `slurp` and the output it produces:

```Clojure
(def file (slurp "my_file.txt"))
println file
```

Output:
```
This is the contents of my_file.txt
```

We can also specify the character encoding of the text file we are reading by passing it as a second argument to the `slurp` function. For example, if our text file is encoded in UTF-8, we can do the following:

```Clojure
(def file (slurp "my_file.txt" :encoding "UTF-8"))
println file
```

Output:
```
This is the contents of my_file.txt
```

## Deep Dive:
Text files have been a fundamental method of storing and sharing data since the early days of computing. They are simple, easily readable, and can be opened and edited by a variety of programs. In Clojure, using the `slurp` function is the most common way to read a text file. However, there are other third-party libraries available that provide additional options for handling text files, such as `clojure.java.io` and `clojure.contrib.io`.

Additionally, when using `slurp`, it is important to note that the entire contents of the file are read into memory as a string. This means that reading large text files can potentially use a lot of memory and slow down the performance of your program. In such cases, it may be more efficient to use a streaming approach, which reads the file line by line, rather than loading it all at once.

## See Also:
- [ClojureDocs: slurp](https://clojuredocs.org/clojure.core/slurp)
- [clojure.java.io library](https://clojuredocs.org/clojure.java.io)
- [clojure.contrib.io library](https://clojuredocs.org/clojure.contrib.io)