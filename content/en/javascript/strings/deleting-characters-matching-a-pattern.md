---
date: 2024-01-20 17:42:19.091714-07:00
description: 'How to: Use `replace()` with a regular expression. The `g` flag replaces
  all matches, not just the first.'
lastmod: '2024-03-13T22:45:00.419298-06:00'
model: gpt-4-1106-preview
summary: Use `replace()` with a regular expression.
title: Deleting characters matching a pattern
weight: 5
---

## How to:
Use `replace()` with a regular expression. The `g` flag replaces all matches, not just the first.

```javascript
let message = "S0m3 messy-string_with! unwanted characters.";
let cleanMessage = message.replace(/[0-9_!-]/g, '');
console.log(cleanMessage); // Output: "Sm messystringwith unwanted characters."
```

## Deep Dive
JavaScript has long used regular expressions (`RegExp`) for pattern matching. The `replace()` function is your go-to for modifying strings since its inception in the early days of the language. Alternatives like `split()` and `join()` or using loops to reconstruct strings exist but aren't as succinct.

Here's a breakdown:
- Use `replace()` for straightforward, one-liner solutions.
- Regular expressions provide powerful pattern-matching capabilities.
- Be aware of `RegExp` performance in tight loops or massive strings.

A word on modern practices: patterns like `/[^a-z]/gi` remove anything not a letter, respecting case-insensitivity with the `i` flag. The introduction of template literals in ECMAScript 2015 made complex replacements easier, enhancing readability.

Regular expressions still intimidate some programmers due to their syntax complexity. However, with modern JavaScript evolving, tools and methods like string manipulation functions (`trim()`, `padStart()`, `padEnd()`, etc.) are available to simplify common tasks, potentially without regex.

## See Also
- [MDN Web Docs on replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [RegExr: Learn, build, & test RegEx](https://regexr.com/)
- [JavaScript RegExp Reference](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
