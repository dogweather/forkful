---
title:                "Concatenating strings"
date:                  2024-01-20T17:35:06.473846-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenating strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenating strings means sticking them together end-to-end. We do it to create messages, URLs, or any text with bits & pieces from different sources.

## How to:
In JavaScript, you've got a few ways to concat strings. Old school: `+`. Modern: template literals. Here’s how they look.

**Using the + operator:**
```javascript
let hello = "Hello, ";
let world = "world!";
let greeting = hello + world; 
console.log(greeting); // "Hello, world!"
```

**Using template literals:**
```javascript
let user = "Jane";
let welcomeMessage = `Hi, ${user}! Welcome back.`;
console.log(welcomeMessage); // "Hi, Jane! Welcome back."
```

## Deep Dive
Back in the day, `+` was the way to go, but it got messy with lots of variables. Enter ES6 in 2015, introducing template literals (those backticks `\``). This meant cleaner-looking strings and the ability to toss in variables and expressions right inside your string without breaking a sweat.

**Why `+` can be a pain:**
- Harder to read with multiple variables.
- Easy to miss spaces, leading to squished words.
- Plus, who needs all those pluses?

**Why template literals rock:**
- Readability: Like an English sentence with blanks filled in.
- Multiline support: You can create strings that span multiple lines without `+` or `\n`.
- Expression interpolation: Pop in variables, do math, all in one go.

**Here's multiline and expressions in action:**
```javascript
let apples = 3;
let oranges = 5;
let fruitSummary = `You have ${apples + oranges} pieces of fruit: 
${apples} apples and 
${oranges} oranges.`;
console.log(fruitSummary);
```
Outputs a neatly formatted summary without any `+` acrobatics.

Technically, string concatenation creates a new string every time you use `+`. For the computer, that's like making a whole new candy bar each time you just want to add a peanut. Not very efficient. Template literals are like having a mold where you can plunk all the ingredients in at once – better performance, especially with big strings or in loops.

## See Also
- MDN Web Docs on template literals (for further reading): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
- String methods and properties (useful when dealing with strings): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String
