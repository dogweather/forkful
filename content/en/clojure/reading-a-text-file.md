---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file means fetching data in a type of human-readable format. Programmers do it to retrieve content, whether it's configuration data, input data for processing, or simply to analyze logs.

## How to:

Reading a text file in Clojure is straightforward. Let's use the `slurp` function:

```Clojure
(defn read-file [file-path]
  (slurp file-path))
```

Call the function with the file path:

```Clojure
(read-file "/path/to/your/file.txt")
```

The output is a string containing the file's content:

```Clojure
"This is the content of your file."
```

## Deep Dive

`slurp` in Clojure handles the opening, reading, and closing of the file, making file I/O much simpler to manage. The `slurp` function has been there since Clojure 1.0 released in 2007 (conj.io/store/v1/org.clojure/clojure/1.0.0/clj/clojure.core/slurp).

If you want stream-oriented reading instead of reading the entire file, consider the `line-seq` function. This function lazily reads the file line by line, thus managing memory better for large files.

```Clojure
(with-open [rdr (clojure.java.io/reader "/path/to/your/file.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))
```

## See Also

Further reading to polish your file handling skills in Clojure: 
- [Clojure Docs - slurp](https://clojuredocs.org/clojure.core/slurp)
- [Stackoverflow - Reading a text file in Clojure](https://stackoverflow.com/questions/33488140/reading-a-text-file-in-clojure)