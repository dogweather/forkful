---
title:                "Видалення символів за візерунком"
aliases: - /uk/typescript/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:15.336494-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? / Що таке та навіщо?
Deleting characters matching a pattern means finding specific sets of characters in a string and removing them. Programmers do this for data cleaning, formatting, or preparing input for further processing.

## How to: / Як це зробити:
```TypeScript
function deleteMatchingChars(input: string, pattern: RegExp): string {
  return input.replace(pattern, '');
}

// Example usage:
const originalText = 'Hello, world! Це текст з деякими рядками...123';
const patternToRemove = /[!-9]/g;  // This will match any character from '!' to '9'
const cleanedText = deleteMatchingChars(originalText, patternToRemove);

console.log(cleanedText); // Outputs: 'Hello, world Це текст з деякими рядками'
```

## Deep Dive / Поглиблене вивчення:
Deleting characters matching a pattern isn't new. It dates back to the usage of regular expressions in early programming. Regular expressions offer a powerful way to identify and process text patterns.

Alternatives to using a regular expression include manually iterating over each character (inefficient), or using string-specific API methods (like 'split' or 'indexOf') for simpler cases. But for complex pattern-matching, regex is king.

Implementation details to note:
- The `g` flag in `/[!-9]/g` makes the pattern global, so all matches are replaced, not just the first.
- TypeScript's strong typing helps ensure your functions work predictably.
- Take care with special regex characters - they need to be escaped if you want to match them literally.

## See Also / Дивись також:
- [MDN Web Docs on Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Documentation](https://www.typescriptlang.org/docs/)
- [Regular Expressions Tester](https://regexr.com/) - helps to test and refine your regex patterns before putting them into code.
