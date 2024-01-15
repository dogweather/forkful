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

## Why

String concatenation is an essential concept in JavaScript and is often used when dealing with text or data manipulation. It allows us to combine multiple strings into a single string, making it easier to work with and display data in a desired format.

## How To

Concatenating strings in JavaScript can be achieved in a few different ways. Let's take a look at some examples:

```
// Using the '+' operator
let greeting = "Hello";
let name = "John";
let message = greeting + " " + name + "!"; // Output: Hello John!
```

In this example, we use the '+' operator to combine the 'greeting' and 'name' strings and add an additional space between them. This is a simple and common way to concatenate strings in JavaScript.

```
// Using the concat() method
let sentence = "Today is";
let day = "Monday";
sentence = sentence.concat(" ", day, "."); // Output: Today is Monday.
```

The concat() method can also be used to concatenate strings. It takes in multiple arguments and combines them into a single string. This can be useful when working with longer strings or when we need to add multiple strings together.

```
// Using template literals
let firstName = "Jane";
let lastName = "Doe";
let fullName = `${firstName} ${lastName}`; // Output: Jane Doe
```

Template literals are a newer feature in JavaScript that allows us to create strings with embedded expressions. This makes it easier to concatenate strings and also provides more flexibility in terms of formatting.

## Deep Dive

Behind the scenes, when we concatenate strings in JavaScript, the compiler converts the code into a series of operations that append each string to the existing one. This means that the more strings and concatenations we have, the longer the process takes. It also highlights the importance of choosing the correct method for concatenation, as some may be more efficient than others.

An interesting note about template literals is that they are interpreted and evaluated at runtime. This means that we can even include functions within the literals to manipulate our strings.

Another thing to keep in mind is that when concatenating a number and a string, the number will be converted into a string and concatenated together. However, if we use the '+' operator, the number will be added instead. For example:

```
let num = 5;
console.log("The number is " + num); // Output: The number is 5
console.log("5" + 5); // Output: 55, not 10
```

## See Also

- [MDN Web Docs - String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [MDN Web Docs - Template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [JavaScript Info - Working with strings](https://javascript.info/string)

String concatenation is a fundamental concept in JavaScript and is used in many applications, from simple greeting messages to complex data manipulation. Understanding the different methods and techniques for concatenating strings can greatly improve our coding efficiency and allow us to create dynamic and versatile strings.