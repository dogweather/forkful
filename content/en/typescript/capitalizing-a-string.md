---
title:                "Capitalizing a string"
html_title:           "TypeScript recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# "Capitalize String in TypeScript: A Swift Dive"

## What & Why?
**String Capitalization** is transforming the first letter of a string to uppercase while keeping the remaining characters unchanged. It's done for grammatical correctness, readability, or meeting specific system requirements.

## How to:

Enforcing string capitalization in TypeScript is simple. Here's how you can do it:

```TypeScript
function capitalizeString(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}
console.log(capitalizeString("hey, how are you?")); //--> "Hey, how are you?"
```
The `capitalizeString()` function takes a string, extracts the first character, converts it into uppercase using `toUpperCase()`, concatenates it with the rest of the string (starting from index 1), and returns this new string.

## Deep Dive
**Historical context:** Capitalizing strings can be traced back to the early days of computing. The focus here is meeting grammatical rules in interface messages, documentations, etc., making them more human-friendly.

**Alternatives:** TypeScript has no built-in function for this unlike other languages, but a simple function as illustrated earlier does the trick. With JavaScript libraries like Lodash, you can import and use the `capitalize` method to achieve the same:

```TypeScript
import { capitalize } from 'lodash';

console.log(capitalize("hey, how are you?")); //--> "Hey, how are you?"
```
**Implementation details:** The critical point here is that JavaScript (and consequently TypeScript) strings are immutable. Therefore, when you capitalize a string, you essentially create a new one, leaving the original string untouched.

## See Also
- TypeScript official docs: https://www.typescriptlang.org/docs/
- Lodash library: https://lodash.com/docs/4.17.15#capitalize
- String methods in JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String

Discussing string functions might seem trivial, especially if you're new to TypeScript or programming as a whole; however, these are the building blocks that will help you start creating complex programs. Happy coding!