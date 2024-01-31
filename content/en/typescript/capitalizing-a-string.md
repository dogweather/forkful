---
title:                "Capitalizing a string"
date:                  2024-01-19
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means transforming the first letter of each word to uppercase and the rest to lowercase. Programmers do this for formatting consistency across user interfaces and to ensure proper nouns and titles are correctly displayed.

## How to:

Here’s some quick TypeScript to get you capitalizing strings:

```typescript
function capitalizeString(input: string): string {
  return input.replace(/\w\S*/g, (word) => {
    return word.charAt(0).toUpperCase() + word.substr(1).toLowerCase();
  });
}

// Example usage:
const title = "hello world from TypeScript";
const capitalizedTitle = capitalizeString(title);
console.log(capitalizedTitle); // Output: "Hello World From Typescript"
```

Easy, right? Now go turn those lowercase strings into something fancy!

## Deep Dive

Capitalization has been around since the time of ancient scripts, refining readability. In programming, beyond aesthetic and grammatical correctness, capitalizing strings can be crucial for comparison operations where "Apple" and "apple" may be treated differently.

Alternatives to the `capitalizeString` function might involve libraries like Lodash, offering the `_.startCase` method, or relying on CSS for visual capitalization (`text-transform: capitalize;`). However, CSS doesn't alter the string's actual value, just the display.

JavaScript originally didn't include a built-in method for string capitalization, leaving it to the creativity of devs. The function above uses a regular expression to identify word boundaries `\w\S*`, capitalizes the first letter with `toUpperCase()`, and the rest with `toLowerCase()`.

## See Also

- MDN String Documentation: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- Lodash's `_.startCase` function: [https://lodash.com/docs/#startCase](https://lodash.com/docs/#startCase)
- String.prototype.toLocaleUpperCase (for locale-sensitive transforms): [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleUpperCase)
