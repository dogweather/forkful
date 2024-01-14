---
title:    "Clojure recipe: Writing a text file"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

In the world of programming, there are many different ways to store and manipulate data. But one often overlooked method is by simply writing a text file. While it may seem old-fashioned, there are actually many advantages to using text files in your code.

Text files are human-readable, making them easy to edit and understand. They also have a simple structure, making them easier to work with in certain situations. And best of all, they can be used for various purposes, from storing configuration files to generating reports.

## How To

To write a text file in Clojure, we can use the `spit` function. This function takes in a file path and the content we want to write to the file. Let's take a look at an example:

```Clojure
(spit "myfile.txt" "Hello, world!")
```

Here, we are using `spit` to write the string "Hello, world!" to a file called "myfile.txt". We can also write multiple lines of text by using the newline character `\n`:

```Clojure
(spit "myfile.txt" "Line 1\nLine 2\nLine 3")
```

These are simple examples, but with the power of Clojure, we can use other functions and data structures to create more complex text files.

## Deep Dive

One of the great benefits of text files is their flexibility. In Clojure, we can use functions like `str` and `join` to manipulate data and then write it to a text file. For example, we can create a CSV file by joining a list of data with commas:

```Clojure
(def data [["Name" "Age" "Occupation"]
           ["Jane" "25" "Teacher"]
           ["John" "30" "Engineer"]])

(spit "data.csv" (join "\n" (map #(join "," %) data)))
```

Output:

```
Name,Age,Occupation
Jane,25,Teacher
John,30,Engineer
```

We can also use text files to store configurations for our projects. Instead of hardcoding values, we can read them from a text file and make our code more dynamic:

```Clojure
(def config (slurp "config.txt"))

; config.txt:
; port: 8081
; database: mydb

(def port (get (into {} (filter #(= "port" (first %)) (map #(split % #":") (str/split-lines config)))) "port"))
(def database (get (into {} (filter #(= "database" (first %)) (map #(split % #":") (str/split-lines config)))) "database"))
```

This allows us to easily change our configurations without having to touch our code.

## See Also

- Official Clojure documentation for text file handling: https://clojure.org/reference/reader#_reader_constants
- A handy cheat sheet for Clojure file manipulation: https://gldraphael.com/blog/2017/11/17/how-to-manipulate-files-in-clojure/
- A Clojure library for generating CSV: https://github.com/onyx-platform/onyx-csv