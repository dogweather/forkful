---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

# Working with Substrings in TypeScript

## What & Why?

A substring is a smaller part of a larger string. Substrings are prevalent in code manipulation to parse and format text efficiently, strengthening data validation or splitting data into manageable chunks.

## How to:

In TypeScript, we primarily use `substring()`, `slice()`, and `substr()` to extract substrings.

Let's break down each with their usage:

```TypeScript
let str = "Hello, TypeScript!";

console.log(str.substring(0,5)); // "Hello"
console.log(str.slice(0,5)); // "Hello"
console.log(str.substr(0,5)); // "Hello"
```
All three methods will produce the same output, "Hello". 
- `substring()` and `slice()` methods take start and end index. 
- `substr()` takes start index and length.

### Note:
- Negative index in `slice()` is counted from the end of the string.

```TypeScript

console.log(str.slice(-6)); // "Script!"
```

## Deep Dive

While extracting substrings is a relatively simple topic, there are some interesting details and nuances.

Historically, the `substring()` method came first. Then `substr()` was added to JavaScript, but because of confusion and naming clashes with `substring()`, it was decided to introduce `slice()`, which is more flexible and intuitive. Note: `substr()` is considered deprecated in modern JavaScript/TypeScript but still widely used.

As mentioned, `slice()` distinguishes itself by accepting negative indexes, counting from the end of the string. `substring()` can't handle negative indexes and will throw an error.

These methods don't manipulate the original string, meaning they're non-mutating methods, they return new strings and keep the original one intact.

## See Also

You may find these resources useful for further reading:

1. Official TypeScript Handbook : [Strings](https://www.typescriptlang.org/docs/handbook/2/objects.html)
2. Mozilla Documentation: [Substring vs Slice vs Substr](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring#differences_among_substring(),_substr(),_and_slice())
3. W3Schools : [JavaScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)