---
date: 2024-01-25 20:50:15.262665-07:00
description: "How to: Imagine you've got a string that's wrapped in double quotes,\
  \ like `\"\\\"Hello, World!\\\"\"` and you want the pure, unquoted text. Here's\
  \ a quick\u2026"
lastmod: '2024-03-13T22:45:00.422622-06:00'
model: gpt-4-1106-preview
summary: Imagine you've got a string that's wrapped in double quotes, like `"\"Hello,
  World!\""` and you want the pure, unquoted text.
title: Removing quotes from a string
weight: 9
---

## How to:
Imagine you've got a string that's wrapped in double quotes, like `"\"Hello, World!\""` and you want the pure, unquoted text. Here's a quick JavaScript snippet to free your string from those quote shackles:

```javascript
let quotedString = "\"Hello, World!\"";
let unquotedString = quotedString.replace(/^"|"$/g, '');
console.log(unquotedString); // Output: Hello, World!
```

And if you're dealing with single quotes? Just tweak the regex a bit:

```javascript
let singleQuotedString = "'Hello, World!'";
let unquotedString = singleQuotedString.replace(/^'|'$/g, '');
console.log(unquotedString); // Output: Hello, World!
```

Or what if your string is a mix of both? No sweat:

```javascript
let mixedQuotedString = "\"'Hello, World!'\"";
let unquotedString = mixedQuotedString.replace(/^["']|["']$/g, '');
console.log(unquotedString); // Output: 'Hello, World!'
```

## Deep Dive
Before JSON took over, escaping quotes was a wild west of backslashes and hacks. Early programming languages didn't always play nice with quotes which meant a lot of manual string manipulation. Now, with standardized data formats, removing quotes is often about cleaning up inputs before they're processed as JSON or storing text without formatting conflicts.

Alternatives to `.replace()`? Sure! You could split and join a string on quotes, use slice if you're certain of your quotes' positions, or even regex match to pull out the needed text. It all depends on the context.

But don't forget about edge cases: quotes inside quotes, escaped quotes, and international characters. Think of your string as a potential minefield of exceptions, and tread carefully. Modern JavaScript engines are optimized to handle regex operations efficiently, so they're generally the go-to, but it's always worth checking performance for heavy data processing tasks.

## See Also
Dig deeper into string manipulation and regex:

- Mozilla Developer Network on String.replace(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace
- Regex101 to test your regex patterns: https://regex101.com/
- JSON.org for understanding why we deal with so many quotes in modern web dev: http://json.org/
