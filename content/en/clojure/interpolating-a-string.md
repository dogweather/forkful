---
title:                "Interpolating a string"
html_title:           "Clojure recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Interpolating a string in Clojure allows programmers to combine multiple strings or values into a single string, making it easier to create dynamic or formatted text. It is commonly used in web development, data manipulation, and data formatting tasks. By using interpolation, programmers can save time and reduce the amount of code needed to create complex or variable strings.

## How to:

To interpolate a string in Clojure, simply use the ```str``` function and insert the values or strings you want to combine within curly braces. Here's an example:

```Clojure
(str "Hello, {:name}! Today is {:day}."
     {:name "John" :day "Monday"})
```

This code will output the following string: "Hello, John! Today is Monday."

You can also use interpolation in combination with other functions, such as ```format```, to create more complex or formatted strings. Here's an example:

```Clojure
(format "Hello, %s! Your lucky number is %d." 
        "Mary" 
        (+ 3 2))
```

This code will output the string: "Hello, Mary! Your lucky number is 5."

## Deep Dive:

Interpolation originated in the Perl programming language and has since been adopted by many other languages, including Clojure. It is a useful tool for string manipulation, especially when dealing with large amounts of data or generating dynamic text.

An alternative to interpolation in Clojure is using the ```(str ... ``` function, which concatenates strings without the need for curly braces. However, interpolation offers more flexibility and can save time by allowing for more concise code.

Under the hood, Clojure uses the ```java.text.MessageFormat``` class to implement interpolation. This allows for more advanced features, such as variable substitution and formatting, to be easily integrated into Clojure code.

## See Also:

- Clojure string functions: https://clojuredocs.org/clojure.core/str

- Java MessageFormat class: https://docs.oracle.com/javase/7/docs/api/java/text/MessageFormat.html

- Perl interpolation: https://www.perl.com/pub/2003/03/07/vars.html/