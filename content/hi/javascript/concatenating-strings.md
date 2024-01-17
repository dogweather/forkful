---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Javascript: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

# What & Why?
String concatenation is a process in which two or more strings are combined to create a single string. This is a common practice used by programmers to manipulate and manipulate strings in their code. By concatenating strings, programmers are able to create dynamic and flexible strings that can be used for various purposes, such as displaying messages, creating file names, or building URLs for web requests.

# How to:
```Javascript
// Concatenating two strings using the plus (+) operator
const string1 = "Hello";
const string2 = "world";
const result = string1 + string2; // result = "Hello world"

// Concatenating multiple strings at once
const greeting = "Hi,";
const name = "John";
const age = 25;
const message = greeting + " my name is " + name + " and I am " + age + " years old."; // message = "Hi, my name is John and I am 25 years old."

// Using template literals (covered in a future article) for string concatenation
const food = "pizza";
const topping = "pepperoni";
const order = `I would like a ${food} with ${topping}.`; // order = "I would like a pizza with pepperoni."
```

# Deep Dive:
String concatenation has been used in programming since the early days of computing. In the early years of programming, concatenation was primarily done using special characters or functions specifically designed for concatenation. However, with the evolution of programming languages, string concatenation has become easier and more convenient to use.

In addition to using the plus (+) operator and template literals, there are also alternative methods for concatenating strings in Javascript. These include using the String.concat() method and the array.join() method. However, the most common and efficient way to concatenate strings in Javascript is still using the plus operator and template literals.

When concatenating strings, it is important to keep in mind a few things. Firstly, the order of the strings matters, as the strings will be combined in the same order as they are written in the code. Secondly, the data types of the strings should also be considered, as concatenating a string with a number or boolean value may result in unexpected output. Lastly, it is important to use proper string formatting to ensure the final concatenated string is easy to read and understand.

# See Also:
- [MDN web docs: String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [MDN web docs: Template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)