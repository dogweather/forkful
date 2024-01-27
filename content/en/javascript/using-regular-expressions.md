---
title:                "Using regular expressions"
date:                  2024-01-19
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions, commonly known as regex, are patterns used to match character combinations in strings. Programmers use them for searching, editing, and manipulating text with precision and efficiency.

## How to:
Here's how to rock regex in JavaScript:

```javascript
// Finding a match
const text = "Find the needle in this haystack";
const regex = /needle/;
console.log(text.match(regex));
// Output: ["needle"]

// Replacing a string
const replacedText = text.replace(regex, "banana");
console.log(replacedText);
// Output: "Find the banana in this haystack"

// Testing for a match
const exists = regex.test(text);
console.log(exists);
// Output: true

// Using flags - 'i' for case insensitive matching
const caseInsensitiveRegex = /NEEDLE/i;
console.log(caseInsensitiveRegex.test(text));
// Output: true

// Using groups to extract data
const data = "John: 1234, Jane: 5678";
const groupRegex = /(\w+): (\d+)/g;
let match;
while ((match = groupRegex.exec(data)) !== null) {
  console.log(`${match[1]}'s number is ${match[2]}`);
}
// Output: "John's number is 1234"
// Output: "Jane's number is 5678"
```

## Deep Dive
Regex has been in use since the 1950s, and it's part of most programming languages. While robust for text parsing, regular expressions can be tricky; newcomers often find them cryptic. For simpler tasks, methods like `String.includes()`, `String.startsWith()`, and `String.endsWith()` can serve as alternatives. When performance is key, remember that regex can be slow—use them wisely and consider optimizing with literal strings or loops for matching single characters.

## See Also
- [MDN RegExp](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/RegExp) – In-depth JavaScript regex resource.
- [RegExr](https://regexr.com/) - Tool to learn, build, & test regex.
- [RegexOne](https://regexone.com/) - Interactive regex tutorials for beginners.
