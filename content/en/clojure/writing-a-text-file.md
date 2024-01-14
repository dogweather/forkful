---
title:                "Clojure recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Text files are a fundamental form of data storage and are essential for any type of programming. They allow for easy storage and retrieval of information, making them a useful tool for developers.

## How To

Writing a text file in Clojure is relatively simple. First, we must define the file path and name where we want to save the text file. In this example, we will create a file called "data.txt" in the current directory.

```Clojure
(def file-path "data.txt")
```

Next, we need to create a writer using the "write-all" function. This function takes two arguments: the file path and the content we want to write to the file.

```Clojure
(with-open [writer (writer file-path)]
  (write-all writer "This is an example text file output"))
```

We can also use the "spit" function to write content to a file. This function takes the file path as the first argument and the content as the second argument.

```Clojure
(spit file-path "This is another example text file output")
```

After executing these functions, a text file will be created in the current directory with the specified content.

## Deep Dive

When writing a text file, it's important to consider the encoding and line endings for the file. Clojure uses the default JVM encoding, which is typically UTF-8, for writing text files. However, we can also specify a different encoding by passing it as a third argument to the "with-open" or "spit" functions.

Furthermore, we can also specify different line endings using the ":newline" option with the "spit" function. The default line endings for Clojure are platform-specific (i.e. "\n" for Unix and "\r\n" for Windows), but we can change this by setting the :newline option to "\n" for Unix or "\r\n" for Windows.

```Clojure
(spit file-path "This text will have custom line endings"
  :newline "\r\n") ;; for Windows line endings
```

## See Also

- Official Clojure documentation on "write-all" and "spit" functions: https://clojuredocs.org

- How to read a text file in Clojure blog post: <link-to-read-a-text-file-article>