---
title:                "Printing debug output"
html_title:           "Clojure recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
Debugging is an essential part of the coding process and being able to print debug output is a helpful tool in identifying and resolving issues. With Clojure's ability to easily print values, it becomes an efficient way to spot errors and track code execution.

## How To
To print debug output in Clojure, we can use the `println` function. This function allows us to print a given value to the console. Let's take a look at an example:

```
Clojure
(defn calculate [x y]
  (println "Calculating..." x "+" y)
  (+ x y))

(calculate 5 5)
```

In the above code, we have defined a function `calculate` that takes in two parameters `x` and `y`. We have also added a `println` statement within the function that will print out the values of `x` and `y` along with the text "Calculating...". Finally, we call the function with the values 5 and 5, which will return 10 and print out "Calculating... 5 + 5" in the console.

This simple yet powerful function can be used throughout your code to print out values, helping you track the flow of data and identify any issues.

## Deep Dive
In some cases, you may want to customize the output of the `println` function. Clojure provides additional options to achieve this, such as using the `format` function or the `str` function.

The `format` function allows us to format our debug output according to a defined template, similar to `printf` in other programming languages. Let's see an example:

```
Clojure
(defn display [name age]
  (println (format "My name is %s and I am %d years old." name age)))

(display "John" 25)
```

In the above code, we have a function `display` which takes in a name and age as parameters. We use the `format` function to define a template and pass in the `name` and `age` parameters to be inserted into the template. This will result in the output "My name is John and I am 25 years old." being printed to the console.

Another option is to use the `str` function, which allows us to concatenate multiple values and strings together. Let's see an example:

```
Clojure
(defn display [name age]
  (println (str "My name is " name " and I am " age " years old.")))

(display "John" 25)
```

In this example, we use the `str` function to concatenate the strings and variables. The output will be the same as the previous example.

## See Also
- [Official Clojure Documentation](https://clojure.org/guides/learn/functions_and_datatypes)
- [Debugging in Clojure](https://dev.to/victoria/debugging-clojure-5442)
- [Mastering Clojure's `println` function](https://www.theserverside.com/blog/Coffee-Talk-Java-News-Stories-and-Opinions/Clojure-printing-for-your-debugging)