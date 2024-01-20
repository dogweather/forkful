---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Let's Talk JavaScript String Interpolation

## What & Why?

String interpolation is the process of injecting variables or expressions into a piece of text - "String". Why the fuss? It's a shortcut to concatenate or organize strings without all the plus signs, spaces and quotes - makes code easier to write and read.

## How to:

In JavaScript, you can perform string interpolation with back-ticks and `${}`. Here's how:

```Javascript
let planet = "World";
console.log(`Hello, ${planet}!`);
```
Output: 
```
Hello, World!
```
But not just variables, you can interpolate expressions. Watch this:

```Javascript
let quantity = 5;
let fruit = "apples";
console.log(`I bought ${quantity * 2} ${fruit} today.`);
```
Output: 
```
I bought 10 apples today.
```
Voila! No fuss of `+` and `" "`, but tidy outputs. 

## Deep Dive

Javascript introduced string interpolation, AKA template literals, in ES6 (ECMAScript 2015) to make life easier for developers. Before that, we used clumsy concatenation:

```Javascript
var fruit2 = "oranges";
console.log("I bought some " + fruit2 + " today.");
```
Output: 
```
I bought some oranges today.
```
Alternative ways do exist. For instance, we have the `concat()` method. Yet, string interpolation is the prevailing practice because of simplicity and readability.

Implementing string interpolation involves HTML-like coding, but don't worry - the curly brackets and dollar signs `${}` never confuse with your HTML. They tell JavaScript to interpret its content as a variable or expression.

Glad to know, right? Now go sprinkle some magic on your string world.


## See Also

For more details about string interpolation in JavaScript:

- [Mozilla Developer’s Network (MDN) - Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- [W3Schools - Using backticks (` `) in JavaScript](https://www.w3schools.com/js/js_string_templates.asp)
- [JavaScript.info - Template literals (back-ticks)](https://javascript.info/string#template-literals)