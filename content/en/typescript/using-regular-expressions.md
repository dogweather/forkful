---
title:                "Using regular expressions"
date:                  2024-01-19
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions, or regex, is a powerful pattern matching and searching tool in programming. Programmers use regex for tasks like validating user input, searching text, or manipulating strings because it's efficient and versatile.

## How to:

Let's jump into TypeScript and see how regex is used for common tasks.

```TypeScript
// Define a regex pattern for an email address
const emailPattern = /\S+@\S+\.\S+/;

// Test if a string matches the email pattern
const email = "user@example.com";
console.log(emailPattern.test(email)); // Output: true

// Find and replace digits in a string
const replaceDigits = "Item 25 costs $30".replace(/\d+/g, '#');
console.log(replaceDigits); // Output: "Item # costs $#"

// Extracting specific parts from a string using capture groups
const data = "April 10, 2021";
const datePattern = /(\w+) (\d+), (\d+)/;
const [, month, day, year] = datePattern.exec(data) || [];
console.log(month, day, year); // Output: "April" "10" "2021"
```

## Deep Dive

Back in the 1950s, mathematician Stephen Kleene described regular expressions as a model to represent regular languages, which later became essential in computer science. Fast forward, regex is ubiquitous in programming for dealing with text.

While regex is a Swiss Army knife for string operations, it's not without alternatives. Depending on the complexity of the task, sometimes string methods like `includes()`, `startsWith()`, `endsWith()`, or even parsing with a library can be better. For example, parsing a complex JSON string using regex can be a nightmare—use a JSON parser instead.

Regarding implementation, regex in JavaScript and TypeScript is based on the ECMAScript language specification. Under the hood, engines use state machines to efficiently match patterns. It's worth noting that regex operations can get expensive in terms of performance, especially with poorly written patterns—watch out for "catastrophic backtracking".

## See Also

- MDN Web Docs on Regular Expressions: [MDN Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Regex101: A tool to test and debug regex patterns [Regex101](https://regex101.com/)
- "Mastering Regular Expressions" book for in-depth understanding: [O'Reilly](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
