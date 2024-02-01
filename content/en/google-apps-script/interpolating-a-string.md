---
title:                "Interpolating a string"
date:                  2024-02-01T13:42:10.182196-07:00
model:                 gpt-4-0125-preview
simple_title:         "Interpolating a string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/google-apps-script/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String interpolation is about embedding expressions within string literals to create new, dynamic strings. Programmers do this to concatenate variables and literals efficiently, making code more readable and easier to maintain.

## How to:

In Google Apps Script, the good old way of concatenating strings and variables is using the `+` operator. But, Google Apps Script, being a dialect of modern JavaScript (ECMAScript), offers template literals for string interpolation, which is much cleaner. Let’s dive into some examples:

```Google Apps Script
// Old school concatenation
var user = "Kim";
var message = "Hello, " + user + "! Welcome back.";
Logger.log(message);  // Outputs: Hello, Kim! Welcome back.

// Using string interpolation with template literals
var user = "Kim";
var message = `Hello, ${user}! Welcome back.`;
Logger.log(message);  // Outputs: Hello, Kim! Welcome back.
```

Template literals are delimited by backticks (`` ` ``) instead of single (' ') or double (" ") quotes. Expressions for interpolation are inserted into `${expression}` placeholders.

Let's try a more complex example:

```Google Apps Script
var ordQty = 3;
var ordItem = "notebooks";
var ordPrice = 5.50;

var ordSummary = `You ordered ${ordQty} ${ordItem}. Total: $${ordQty * ordPrice}`;
Logger.log(ordSummary);  // Outputs: You ordered 3 notebooks. Total: $16.5
```

These examples highlight how interpolated strings can simplify the process of constructing strings from variables and expressions.

## Deep Dive

String interpolation has been around in many programming languages before making its debut in JavaScript with ES6, hence also available in Google Apps Script. Before ES6, developers had to rely on less elegant methods like string concatenation which could easily lead to hard-to-read code, especially with multiple variables or complex expressions.

In the realm of Google Apps Script, using string interpolation not only makes the code cleaner and more readable but also reduces the chance of making typos or errors in strings. It's particularly useful when dealing with dynamic content that needs to be generated within scripts for applications like Google Sheets, Docs, or sending automated emails where personalized content is key.

However, it's essential to note that while string interpolation offers convenience and readability, it might not always be the best choice for highly performance-sensitive applications. In scenarios where performance is critical, and strings are concatenated in a highly repetitive or large-scale manner, traditional methods or newer alternatives like `Array.join()` might offer slight performance benefits. Yet, for the vast majority of use cases in Google Apps Script, the elegance and simplicity of string interpolation make it an excellent choice for developers.
