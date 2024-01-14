---
title:    "Clojure recipe: Reading a text file"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Reading a text file is a fundamental task in many programming languages, and Clojure is no exception. Whether you are looking to extract data from a file or simply want to display its contents, understanding how to read a text file in Clojure can greatly enhance your coding abilities.

## How To

Reading a text file in Clojure is a relatively straightforward process. Let's take a look at an example where we read the contents of a file called "sample.txt" and print it out:

```Clojure
(with-open [f (clojure.java.io/reader "sample.txt")]
  (println (slurp f)))

```

In this code block, we first use the `clojure.java.io/reader` function to open our text file and assign it to the variable `f`. Then, using the `with-open` function, we ensure that our file is automatically closed when we are done reading it. Finally, we use the `println` function to print out the contents of the file using the `slurp` function.

If we run this code, we should see the contents of the "sample.txt" file printed out in the console. However, if the contents of the file are too large to be displayed in one line, we can use the `line-seq` function to iterate through each line of the file and print them out separately. Let's take a look:

```Clojure
(with-open [f (clojure.java.io/reader "sample.txt")]
  (line-seq f))

```

Running this code will now show each line of the file printed out separately, making it easier to read and manipulate.

## Deep Dive

While the above examples cover the basics of reading a text file in Clojure, there are other useful functions and techniques that can be used.

One such function is `spit`, which allows us to write data to a file. This can be useful when we want to write the output of our code to a text file. Let's take a look at an example:

```Clojure
(def data "This is a sample of data")

(spit "output.txt" data)

```

In this code, we first define a variable `data` with some sample text. Then, using the `spit` function, we write the contents of `data` to a new file called "output.txt". Running this code will create a new text file with the specified data.

Additionally, we can also use the `clojure.string/split-lines` function to split a file's contents into separate lines, which can then be manipulated as needed. This function can be useful if we only want to work with specific lines of a file.

## See Also

- [Official Clojure Documentation on reading and writing files](https://clojure.org/guides/learn/syntax#_reading_an_file)
- [ClojureDocs example of reading a large text file](https://clojuredocs.org/clojure.core/slurp#example-542692cd0364486313e322e5)
- [ClojureDocs example of using `spit`](https://clojuredocs.org/clojure.core/spit#example-542692cd0364486313e322e5)