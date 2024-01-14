---
title:                "Javascript recipe: Concatenating strings"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

In the world of programming, we often encounter situations where we need to combine different strings together to create a meaningful output. This process of combining strings is known as concatenation and is a fundamental concept in Javascript programming. Whether you are a beginner or an experienced coder, understanding how to concatenate strings can greatly improve your coding skills and make your code more efficient.

## How To

Concatenating strings in Javascript is a relatively simple process. In order to combine two or more strings, we use the "+" operator. Let's see how it works with the following example:

```Javascript
let firstName = "John";
let lastName = "Smith";
let fullName = firstName + " " + lastName;

console.log(fullName);
```

The output of this code would be: "John Smith". As you can see, we first declared two variable, "firstName" and "lastName", and then used the "+" operator to join them together with a space in between. This resulted in our final output, which is the full name of the person.

We can also concatenate strings using a shortcut notation, known as the "+=" operator. Let's take a look at an example:

```Javascript
let greeting = "Hello";
greeting += ", how are you?";

console.log(greeting);
```

The output of this code would be: "Hello, how are you?". In this example, we first declared a variable called "greeting" and assigned it the value of "Hello". Then, we used the "+=" operator to append the string ", how are you?" to the existing value of "greeting", resulting in our final output.

It is important to note that when concatenating strings, we need to pay attention to the data types. If any of the values involved are numbers, they will be converted to strings before concatenation occurs. Let's see an example:

```Javascript
let x = 5;
let y = "3";

console.log(x + y);
```

The output of this code would be: "53". Here, the number 5 was converted to a string and then concatenated with the string "3", resulting in our final output.

## Deep Dive

In addition to using the "+" and "+=" operators, there are other ways to concatenate strings in Javascript. One of them is by using template literals, which allow for easier string interpolation. Let's take a look at an example:

```Javascript
let firstName = "John";
let lastName = "Smith";

console.log(`My name is ${firstName} ${lastName}.`);
```

The output of this code would be: "My name is John Smith.". As you can see, we used the backtick character (`) to enclose our string. Within the string, we used the "${}" notation to insert the values of the variables "firstName" and "lastName", resulting in our final output.

Another important thing to note is that we can concatenate strings with other data types as well, such as arrays and objects. This can allow for more complex and dynamic outputs. However, it is important to pay attention to the specific syntax and methods used for concatenation with these data types.

## See Also

Now that you have a better understanding of concatenating strings in Javascript, here are some additional resources you can check out to further expand your knowledge:

- [MDN Web Docs: String Concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition#string_concatenation)
- [W3Schools: JavaScript Strings](https://www.w3schools.com/js/js_strings.asp)
- [Codecademy: Concatenation](https://www.codecademy.com/courses/introduction-to-javascript/lessons/strings/values-actions)