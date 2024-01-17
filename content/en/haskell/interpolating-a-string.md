---
title:                "Interpolating a string"
html_title:           "Haskell recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolating a string in Haskell is the act of inserting a value into a string at a specific location. It allows programmers to create dynamic strings, rather than static ones, making their code more flexible and versatile. This is especially useful when dealing with user input, database queries, or any other situation where the content of a string may change.

## How to:
To interpolate a string in Haskell, we first need to use the "intercalate" function from the "Data.List" module, which joins a list of strings with a separator. Then, we can use the "$" operator to apply a function to a value. Let's see an example of interpolating the name "John" into a greeting:

```Haskell
import Data.List

greet name = "Hello " ++ name ++ "!"

"Hello John!"
```

In the code above, we use "Hello" as the first part of our string, then use the "++" operator to join it with the value of the "name" variable, and finally add an exclamation mark at the end. To make it more interesting, let's say we want to also include the current year in our greeting:

```Haskell
import Data.List

greet name = "Hello " ++ name ++ "! We are currently in the year: " ++ show(year)

"Hello John! We are currently in the year: 2021"
```

By using the "show" function, we can convert the "year" value to a string and insert it into our greeting. This allows our code to be more dynamic and adaptable in different situations.

## Deep Dive:
The concept of string interpolation has been around for a long time and is not exclusive to Haskell. Other programming languages, such as Java, Python, and Ruby, also have their own methods of interpolating strings. These methods vary in syntax and functionality, but the ultimate goal is the same: to insert a value into a string.

In Haskell, the "intercalate" function is just one way to interpolate a string. There are other functions and methods, such as using the "printf" function from the "Text.Printf" module, which allows for more advanced formatting of strings. Additionally, for more complex scenarios, there are libraries available, such as "Text.InterpolatedString.Perl6", which offers a Perl6-like interpolation syntax.

## See Also:
- [Haskell Documentation for Data.List](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html)
- [Haskell Documentation for Text.Printf](https://hackage.haskell.org/package/base-4.15.0.0/docs/Text-Printf.html)
- [InterpolatedString-Perl6 Library on Hackage](https://hackage.haskell.org/package/InterpolatedString-Perl6)