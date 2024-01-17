---
title:                "Finding the length of a string"
html_title:           "Clojure recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string is a common task in programming that involves determining the number of characters in a given string. This is helpful for a variety of reasons, such as validating user input, formatting text, and manipulating data.

## How to:

To find the length of a string in Clojure, we can use the built-in function ```count```. We simply pass the string as an argument and the function will return the length as an integer. Here's an example:

```Clojure
(count "Hello World") ; Output: 11
```

We can also use the ```len``` function, which works the same way as ```count```. However, ```len``` can be used for other data types such as lists and vectors. Here's an example:

```Clojure
(len [1 2 3 4 5]) ; Output: 5
```

## Deep Dive:

The concept of finding the length of a string has been around since the early days of computer programming. However, in older programming languages, it was a more complex task that required writing custom functions. With the advancement of modern programming languages, this task has become much simpler with built-in functions like ```count``` and ```len```.

In some programming languages, the ```length``` function is used instead of ```count``` or ```len```. However, they all achieve the same result of finding the length of a string. It is just a matter of preference and compatibility with the specific programming language.

When finding the length of a string, it is important to note that it includes all characters, including spaces, punctuation, and special characters. This may affect the output and should be considered when using the length in other parts of the code.

## See Also:

- Official Clojure documentation on ```count```: https://clojure.org/api/cheatsheet
- Alternative functions for finding the length of data: ```len``` and ```length```.
- Other useful string manipulation functions in Clojure: ```substring```, ```split```, and ```replace```.