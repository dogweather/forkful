---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings means pulling out smaller pieces of a larger string. Programmers do this to handle or manipulate specific portions of the text—handy when you're dealing with user input, file data, or even massive DNA sequences!

## How to:

In JavaScript, you can extract substrings by using a few different methods: `substring()`, `substr()`, or `slice()`. Each returns a new string without changing the original string.

```Javascript 
let str = "Hello, world!";

console.log(str.substring(0,5)); // "Hello"
console.log(str.substr(0,5));    // "Hello"
console.log(str.slice(0,5));     // "Hello"
```

They work a bit differently though. Here's an example:

```Javascript 
let str = "Hello, world!";

console.log(str.substring(-3));  // "Hello, world!", negative arguments are treated as 0
console.log(str.substr(-3));     // "ld!", negative first argument means starting backwards from the end
console.log(str.slice(-3));      // "ld!", and same for slice method
```

## Deep Dive:

**Historical context** - Extracting substrings has been a fundamental part of programming since its early days —because really, what's programming without handling text?

**Alternatives** - Besides `substring()`, `substr()`, and `slice()`, you can also use `split()` to break the string at certain points and choose parts to work with. Combine with `join()` can yield similar results.

```Javascript 
let str = "Hello, world!";
console.log(str.split(', ')[0]); // "Hello"
```

**Implementation details** - `substring()` and `slice()` are similar, but treat negatives differently. `substr()` treats its second argument as a length, not an ending index. It’s also considered a legacy function and might not be present in future versions of JavaScript.

## See Also:

For more on strings in JavaScript, check out these links:
- [MDN Web Docs - String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
- [JavaScript.info - Strings](https://javascript.info/string)
- [W3Schools - JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)