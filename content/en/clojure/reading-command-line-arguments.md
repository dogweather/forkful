---
title:                "Clojure recipe: Reading command line arguments"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

If you're a developer working with Clojure, you may already be familiar with the power and flexibility of the language. But what about when it comes to handling command line arguments? Understanding how to read and use these arguments can greatly enhance your programming skills and make your applications more versatile. In this blog post, we'll dive into the world of command line arguments in Clojure and explore why they are important for any developer.

## How To

Reading command line arguments in Clojure is a straightforward process. First, you'll need to import the `clojure.java.io` and `clojure.string` libraries. Then, you can use the `command-line-args` function to retrieve a list of all the command line arguments passed in when your program is executed. Let's take a look at an example:

```Clojure
(require '[clojure.java.io :as io])
(import 'java.io.File)
(use '[clojure.string :only [join]])

(def input-file (File. (first (command-line-args))))
(def lines (io/reader input-file))
(def content (join ", " (line-seq lines)))
(println content)
```

In this example, we first import the necessary libraries and then use `command-line-args` to retrieve the first argument, which will be the input file that we want to read. We then use `io/reader` to convert the file into a sequence of lines, and finally use `join` to combine the lines into a single string. Finally, we print out the content of the file.

If we execute this program with the command `java -cp "myprogram.jar" input.txt`, we will see the contents of the input file printed out to the console.

## Deep Dive

Now that we've gone over the basics of reading command line arguments in Clojure, let's take a deeper dive into how these arguments are actually passed in and how we can access them.

When running a program from the command line, arguments are passed in after the program name. This is known as the "args" vector. For example, if we run `java -cp "myprogram.jar" input.txt`, the args vector will contain `["input.txt"]`. We can access these arguments using the `command-line-args` function as shown in the example above.

It is important to note that command line arguments are always passed in as strings. So if you want to use the arguments as numbers or booleans, you will need to convert them using functions such as `Integer/parseInt` or `Boolean/parseBoolean`.

Additionally, we can pass in multiple arguments separated by spaces. These arguments will then be stored in the args vector as separate strings, allowing us to access and use them individually in our program.

## See Also

Now that you have a better understanding of how to read command line arguments in Clojure, here are some additional resources that you may find helpful:

- [ClojureDocs: command-line-args function](https://clojuredocs.org/clojure.java.io/command-line-args)
- [Official Clojure documentation on I/O](https://clojure.org/reference/io)
- [Article on manipulating strings in Clojure](https://www.baeldung.com/clojure-string-manipulation)

Happy coding!