---
date: 2024-01-20 17:43:16.529222-07:00
description: "How to: Historically, dealing with strings in programming can trace\
  \ its roots back to the dawn of computing. In TypeScript, which builds upon JavaScript,\u2026"
lastmod: '2024-04-05T21:53:35.532452-06:00'
model: gpt-4-1106-preview
summary: Historically, dealing with strings in programming can trace its roots back
  to the dawn of computing.
title: Deleting characters matching a pattern
weight: 5
---

## How to:
```TypeScript
function deletePattern(text: string, pattern: string): string {
  // Create a RegExp from the pattern string
  const regex = new RegExp(pattern, 'g');
  // Replace occurrences of the pattern with an empty string
  return text.replace(regex, '');
}

// Example usage
const originalText = "Hello, World! This -- is a test.";
const newText = deletePattern(originalText, "[,\\-!]");
console.log(newText);  // Output: "Hello World This  is a test"
```

## Deep Dive
Historically, dealing with strings in programming can trace its roots back to the dawn of computing. In TypeScript, which builds upon JavaScript, manipulating strings is a daily task. The `replace()` function we've used is inherited from JavaScript's robust string manipulation arsenal.

There are alternatives to RegExp for matching patterns – sometimes you might want to manually iterate through each character and make decisions with a switch statement or a series of ifs. But regular expressions provide a concise and powerful way to describe complex patterns for matching.

Implementation details get interesting when you dive into how RegExp patterns are interpreted at runtime. The 'g' flag in the RegExp constructor tells the engine to search globally across the string. Without it, only the first match would be replaced. Regular expressions can be simple or mind-bogglingly complex, depending on your needs.

## See Also
- The MDN Web Docs on RegExp: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp)
- TypeScript Handbook on string manipulation: [https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- Regular expressions tester to help with pattern creation: [https://regexr.com/](https://regexr.com/)
