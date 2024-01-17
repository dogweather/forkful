---
title:                "एक स्ट्रिंग इंटरपोलेशन"
html_title:           "TypeScript: एक स्ट्रिंग इंटरपोलेशन"
simple_title:         "एक स्ट्रिंग इंटरपोलेशन"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpolating a string, also known as String Interpolation, is a way of inserting dynamic values into a string in TypeScript. Programmers use string interpolation to make their code more readable and concise by avoiding the tedious concatenation of strings and variables. It also makes code easier to maintain and debug.

## How to:
To interpolate a string in TypeScript, use the backtick (```) character instead of the single or double quotes. Then, to insert a dynamic value, use a dollar sign ($) followed by curly braces ({}) containing the variable or expression you want to insert. Here's an example:

```TypeScript
const name = "John";
console.log(`Hello ${name}!`); 
```
Output: Hello John!

You can also use string interpolation with functions and object properties. Here's an example:

```TypeScript
function add(x: number, y: number) {
    return x + y;
}

console.log(`The sum of 2 and 3 is ${add(2,3)}.`);

let person = {
    name: "Mary",
    age: 25
};

console.log(`My name is ${person.name} and I am ${person.age} years old.`);
```
Output: The sum of 2 and 3 is 5.
My name is Mary and I am 25 years old.

## Deep Dive:
String interpolation was first introduced in EMCA Script 6 (ES6) and is now supported by TypeScript and most modern programming languages. Before string interpolation, programmers had to use string concatenation, which resulted in lengthy and cumbersome code. String interpolation improved the readability and efficiency of code by allowing programmers to easily insert dynamic values into strings.

An alternative to string interpolation is using string templates, which are more flexible and allow for multiple lines of code. However, string templates have a slightly different syntax and cannot contain expressions or variables.

To implement string interpolation, TypeScript uses Template Literals, which allow for embedded expressions and multi-line strings. The expressions inside the curly braces are evaluated and converted to strings before being inserted into the final string.

## See Also:
- [TypeScript String Interpolation Documentation] (https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- [ES6 Template Literals] (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [Differences between String Interpolation and String Templates] (https://stackoverflow.com/questions/31961868/difference-between-interpolation-and-template-literal-in-es6)