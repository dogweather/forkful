---
title:                "Finding the length of a string"
html_title:           "Elixir recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string means determining the number of characters in a given string. This is a common and crucial task for programmers, as it allows them to manipulate and validate user inputs, calculate data sizes, and perform various string operations.

## How to:
To find the length of a string in Elixir, we can use the `String.length` function. This function takes in a string as an argument and returns the number of characters in the string. See the example below:

```Elixir
string = "Hello, world!"
String.length(string)
```
Output:
```
13
```
In addition to the `String.length` function, we can also use the `length/1` function, which takes in a list as an argument and returns the size of the list. Since strings in Elixir are essentially lists of characters, this function can also be used to find the length of a string. See the example below:

```Elixir
string = "Hello, world!"
length(string)
```
Output:
```
13
```

## Deep Dive:
Historically, finding the length of a string has been a task that required manual counting by the programmer. However, with the advancement of programming languages, built-in functions like `String.length` and `length/1` have made this task much easier and faster. Alternatives to using these functions include using regular expressions or writing custom functions to iterate through the string and count the characters.

To understand how finding the length of a string works, it's important to know that strings in Elixir are encoded as UTF-8, which is a variable-width character encoding. This means that characters can have different byte sizes, making it crucial to use a built-in function to accurately find the length of a string.

## See Also:
- [Elixir-lang.org](https://elixir-lang.org/docs/stable/elixir/String.html#length/1) for more information on the `String.length` function.
- [Elixir-lang.org](https://elixir-lang.org/docs/stable/elixir/Kernel.html#length/1) for more information on the `length/1` function.