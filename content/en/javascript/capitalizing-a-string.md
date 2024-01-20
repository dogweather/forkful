---
title:                "Capitalizing a string"
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means changing the first character of a word to an uppercase letter. Programmers do it to follow language conventions, improve readability, or format text like titles.

## How to:
JavaScript doesn't have a built-in method for capitalizing, but here's a simple function that does the trick:

```javascript
function capitalizeFirstLetter(string) {
    return string.charAt(0).toUpperCase() + string.slice(1);
}

console.log(capitalizeFirstLetter('hello')); // Output: Hello
```

For multiple words:

```javascript
function capitalizeWords(str) {
    return str.split(' ').map(capitalizeFirstLetter).join(' ');
}

console.log(capitalizeWords('hello world!')); // Output: Hello World!
```

## Deep Dive
Capitalizing strings didn't always have built-in functions in languages, often involving manual ASCII manipulation. Nowadays, most programming languages offer methods for string manipulation, but JavaScript requires a more DIY approach.

### Alternatives:
You could use CSS to capitalize text for web pages (`text-transform: capitalize;`), or libraries like Lodash have capitalize functions. But doing it with plain JavaScript, as shown above, has no dependencies.

### Implementation Details:
`charAt(0)` grabs the first character. `toUpperCase()` makes it uppercase. Combining it with the rest of the string `slice(1)` gives you a capitalized string. This method works well assuming the input is a string and doesn't start with a space.

## See Also:
- MDN's text-transform CSS for capitalization: https://developer.mozilla.org/en-US/docs/Web/CSS/text-transform
- Lodash's capitalize method documentation: https://lodash.com/docs/4.17.15#capitalize
- JavaScript String.prototype.toUpperCase(): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase