---
title:                "Javascript recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to combine multiple strings together into one? Maybe you wanted to display a full name, or create a sentence with varying words. That's where string concatenation comes in handy! It allows you to join strings together to create a new, longer string.

## How To

To concatenate strings in Javascript, you can use the "+" operator. Let's take a look at an example:

```Javascript
let firstName = "John";
let lastName = "Smith";

let fullName = firstName + " " + lastName;

console.log(fullName);
```

In this example, we first declare two variables, "firstName" and "lastName" which contain the strings "John" and "Smith". Then, we use the "+" operator to combine the two variables and a space in between to create a new string called "fullName". Finally, we use the "console.log()" function to output the value of "fullName" which is "John Smith".

You can also use concatenation to create sentences or phrases with dynamic information, such as a user's input. Let's see an example:

```Javascript
let animal = prompt("What is your favorite animal?");
let sound = prompt("What sound does that animal make?");

let sentence = "The " + animal + " says " + sound + "!";

console.log(sentence);
```

In this code, we use the built-in "prompt()" function to ask the user for their favorite animal and the sound it makes. Then, we use concatenation to combine those inputs with a predetermined sentence structure. For example, if the user inputs "lion" and "roar", the output would be "The lion says roar!".

## Deep Dive

Behind the scenes, concatenating strings in Javascript involves converting each piece of data into a string before joining them together. This is done using the "toString()" method. For example, when we used the "+" operator to combine our variables, "firstName" and "lastName", they were first converted to strings before being joined together.

It's also important to note that concatenating strings is non-destructive, meaning it does not change the original values of the variables being combined. It simply creates a new string with the combined values.

## See Also

For more information on string concatenation, check out these resources:

- [MDN Web Docs on string concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)
- [W3Schools tutorial on string concatenation](https://www.w3schools.com/js/js_string_concat.asp)
- [Concatenating strings in different ways in Javascript](https://www.codementor.io/@mattgoldspink/javascript-best-practices-string-concatenation-6m1jzq9mf)