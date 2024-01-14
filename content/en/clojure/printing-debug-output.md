---
title:                "Clojure recipe: Printing debug output"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

When writing code, developers often encounter bugs or unexpected behavior. One way to troubleshoot these issues is by printing out debug output. This can help identify the problematic areas of the code and allow for more efficient debugging.

## How To

To print debug output in Clojure, you can use the `println` function. This function takes in any number of arguments and prints them to the console.

```Clojure
(def num 10)
(println "The value of num is:" num)
```

This will print out "The value of num is: 10" to the console. Additionally, you can use the `prn` function to print the output without any additional formatting.

```Clojure
(def nums [1 2 3])
(prn "The contents of nums are:" nums)
```

This will print out "The contents of nums are: [1 2 3]" to the console. Another useful function for printing debug output is `pprint`, which allows for more readable and structured output.

```Clojure
(def person {:name "John" :age 25})
(pprint "The person's details are:")
(pprint person)
```

This will print out:

```
"The person's details are:"
{:age 25, :name "John"}
```

## Deep Dive

When using `println` or `prn`, it is important to keep in mind that they add a new line character at the end of the output. This can be suppressed by using `print` instead. Additionally, you can use the `format` function to control the formatting of the output.

```Clojure
(def key :my-key)
(format "The key is: %s" key)
```

This will print out "The key is: my-key" to the console. The `%s` in the format string is a specifier that represents a string value. There are other specifiers available for different data types such as integers, floats, and characters. You can also use the `printf` function, which works the same as `print` but accepts a format string and arguments.

## See Also

For more information on printing debug output in Clojure, check out these resources:

- [Official Clojure documentation on I/O](https://clojure.org/reference/io)
- [Clojure for the Brave and True book](https://www.braveclojure.com/debugging/)
- [Clojure Debugging Tips blog post](https://purelyfunctional.tv/guide/clojure-debugging-tips/)