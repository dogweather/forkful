---
date: 2024-01-25 20:50:23.943509-07:00
description: 'How to: Here''s your no-nonsense guide to cutting those pesky quote
  marks loose from your strings in TypeScript.'
lastmod: '2024-03-13T22:44:59.847732-06:00'
model: gpt-4-1106-preview
summary: Here's your no-nonsense guide to cutting those pesky quote marks loose from
  your strings in TypeScript.
title: Removing quotes from a string
weight: 9
---

## How to:
Here's your no-nonsense guide to cutting those pesky quote marks loose from your strings in TypeScript.

```typescript
// Option A: Replace single or double quotes using regex
function removeQuotes(input: string): string {
  return input.replace(/^["']|["']$/g, '');
}

console.log(removeQuotes(`"Quoted string"`)); // Quoted string
console.log(removeQuotes(`'Another one'`)); // Another one

// Option B: Dealing with strings that start and end with different quotes
function removeMismatchedQuotes(input: string): string {
  return input.replace(/^(['"])(.*?)(?<!\1)\1$/, '$2');
}

console.log(removeMismatchedQuotes(`"Mismatched'`)); // "Mismatched'

// Option C: Trimming multiple types of quotes
function removeAllQuotes(input: string): string {
  return input.replace(/['"]+/g, '');
}

console.log(removeAllQuotes(`"'Mix'n'Match'"`)); // Mix'n'Match
```

## Deep Dive
Way back before TypeScript was even a thing, JavaScript coders were already dealing with quote shenanigans, and the story's pretty much the same for TypeScript. As times change, so does the way we slice strings up. Nowadays, with regex's muscle power, we shove aside using clunky string slicing or other tedious methods.

While the above examples should cover most of your needs, remember, quoting can get complex. Nested, mismatched, and escaped quotes are the tricksters waiting to trip you up. For these, you may need more sophisticated patterns or even parsers to handle every curly case. 

Alternatives? Some folks like to go with libraries like lodash, with methods like `trim` and `trimStart` / `trimEnd`, which can be tailored to clip quotes if you set the characters you want to snip.

And for you TypeScript enthusiasts, let's not forget about the types. While here we're dealing mostly with strings, when you're working with user input or parsing, throwing in some type guards or even generics can help ensure you keep your code as safe as your quotes are trimmed.

## See Also
Check out these virtual hotspots for more info:

- MDN Web Docs on regex (https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- TypeScript Official Documentation (https://www.typescriptlang.org/docs/)
- You Don't Need Lodash/Underscore – String Helpers (https://github.com/you-dont-need/You-Dont-Need-Lodash-Underscore#strings)
- Stack Overflow: Traverse the trenches where countless devs have battled quote catastrophes (https://stackoverflow.com/)
