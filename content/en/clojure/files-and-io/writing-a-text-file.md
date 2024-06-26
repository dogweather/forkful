---
date: 2024-02-03 19:03:12.778784-07:00
description: "How to: The `spit` function is the simplest way to write text to a file\
  \ in Clojure. It takes two arguments: the file path and the string to write. If\
  \ the\u2026"
lastmod: '2024-03-13T22:44:59.761934-06:00'
model: gpt-4-0125-preview
summary: The `spit` function is the simplest way to write text to a file in Clojure.
title: Writing a text file
weight: 24
---

## How to:


### Writing text to a file using Clojure's built-in functions
The `spit` function is the simplest way to write text to a file in Clojure. It takes two arguments: the file path and the string to write. If the file doesn't exist, `spit` will create it. If it does, `spit` will overwrite it.

```clojure
(spit "example.txt" "Hello, world!")
```

To append text to an existing file, you can use the `spit` function with the `:append` option.

```clojure
(spit "example.txt" "\nLet's add this new line." :append true)
```

After running these snippets, "example.txt" will contain:

```
Hello, world!
Let's add this new line.
```

### Using third-party libraries
While Clojure's built-in capabilities are often sufficient, the community has developed robust libraries for more complex or specific tasks. For file I/O, one popular library is `clojure.java.io`, which provides a more Java-like approach to file handling.

To use `clojure.java.io` for writing to a file, you first need to import it:

```clojure
(require '[clojure.java.io :as io])
```

Then, you can use the `writer` function to obtain a writer object, and the `spit` function (or others like `print`, `println`) to write to the file:

```clojure
(with-open [w (io/writer "example_with_io.txt")]
  (.write w "This is written using clojure.java.io"))
```

This will create (or overwrite if it already exists) "example_with_io.txt" with the text:

```
This is written using clojure.java.io
```

Remember: `with-open` ensures that the file is properly closed after writing, avoiding potential resource leaks.
