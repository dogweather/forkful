---
title:                "Concatenating strings"
html_title:           "Javascript recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings in programming refers to the process of combining multiple strings into one. It is a common operation used by programmers to build longer strings from smaller ones, for various purposes such as displaying messages and generating dynamic content.

## How to:

To concatenate strings in Javascript, we use the `+` operator. Let's take a look at an example:

```javascript
let string1 = "Hello";
let string2 = "World";
let concatenated = string1 + " " + string2;
console.log(concatenated); // Output: Hello World
```

We can also use template literals, enclosed in backticks, to concatenate strings and add variables or expressions within the string. Here's an example:

```javascript
let name = "John";
let age = 30;
console.log(`Hello, my name is ${name} and I am ${age} years old.`); // Output: Hello, my name is John and I am 30 years old.
```

## Deep Dive:

Concatenating strings has been a part of programming since the early days. In the early days of computers, strings were represented as arrays of characters and concatenation was done by manually looping through each character, which was a tedious process. As programming languages evolved, built-in string concatenation methods were introduced, making it easier for developers to combine strings.

There are also other ways to concatenate strings in JavaScript, such as the `concat()` method or the `join()` method, which can be used on arrays of strings to concatenate them into one string.

Implementing string concatenation in JavaScript is relatively simple, but it is important to pay attention to the order of the strings and variables being combined, as it can affect the output. It's also important to keep in mind that string concatenation can be a resource-intensive process, especially when dealing with large strings, so it's best to use it sparingly when possible.

## See Also:

- [MDN Web Docs on string concatenation in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [W3Schools on string concatenation in JavaScript](https://www.w3schools.com/jsref/jsref_concat_string.asp)
- [FreeCodeCamp on string concatenation in JavaScript](https://www.freecodecamp.org/news/how-to-combine-strings-in-javascript/)