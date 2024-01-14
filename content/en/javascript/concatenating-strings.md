---
title:    "Javascript recipe: Concatenating strings"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

When writing JavaScript code, you may often come across a situation where you need to combine two or more strings together to form a longer string. This process is called concatenation and it is an important concept in string manipulation. By understanding how to concatenate strings, you can better manipulate and display data in your code.

## How To

To concatenate strings in JavaScript, you can use the "+" operator. Let's take a look at a simple example:

```
// Define two string variables
let greeting = "Hello";
let name = "John";

// Concatenate the two strings
let message = greeting + " " + name;

// Output the final message
console.log(message);
```

In the above code, we first defined two string variables, "greeting" and "name." Then, using the concatenation operator "+", we combined the two strings with a space in between to form the message "Hello John." Finally, by using the "console.log()" function, we are able to output the final message.

You can also concatenate strings using the "concat()" method. This is particularly useful if you have more than two strings to concatenate. Take a look at the following example:

```
// Define three string variables
let first = "I";
let second = "love";
let third = "coding!";

// Concatenate the three strings using the concat() method
let sentence = first.concat(" ", second, " ", third);

// Output the final sentence
console.log(sentence);
```

This code will output the sentence "I love coding!" by combining the three strings with spaces in between using the concat() method. 

## Deep Dive

It's important to note that the "+" operator in JavaScript not only concatenates strings, but it can also concatenate other data types such as numbers and booleans. However, when used with non-string data types, it will convert them into strings before concatenation. For example:

```
// Concatenating strings with numbers
let result = "The answer is: " + 5;
console.log(result); // Output: The answer is: 5

// Concatenating strings with booleans
let statement = "I am " + true;
console.log(statement); // Output: I am true
```

In addition, you can also use template literals to concatenate strings in a more efficient way. Template literals use backticks (`) instead of single or double quotes and allow you to include variables or expressions directly in the string without using the "+" operator. For example:

```
// Template literals example
let product = "apple";
let price = 2.99;
let quantity = 3;
let totalPrice = `You bought ${quantity} ${product}s for a total of $${quantity * price}.`;
console.log(totalPrice); // Output: You bought 3 apples for a total of $8.97.
```

In the above code, we used template literals to concatenate multiple strings and expressions to form the final totalPrice message.

## See Also

- [MDN Web Docs: String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [W3Schools: JavaScript String Concatenation](https://www.w3schools.com/js/js_string_concat.asp)
- [FreeCodeCamp: A beginner's guide to JavaScript's string concatenation operator](https://www.freecodecamp.org/news/beginner-guide-javascript-string-concatenation-operator/)