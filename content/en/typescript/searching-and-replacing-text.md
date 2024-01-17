---
title:                "Searching and replacing text"
html_title:           "TypeScript recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text is a common task in programming where specific words or phrases are replaced with new values. This can be done for various reasons, such as correcting spelling errors, updating code to meet new requirements, or simply making code more efficient.

## How to:
To search and replace text in TypeScript, the built-in `replace()` method can be used. This method takes in two parameters: the text to be replaced and the new text to replace it with. Here's an example of how it can be used:

```
// Define a string with the text to be replaced
let str: string = "Hello world! This is a sample string for demonstration purposes.";

// Use the replace() method to replace "sample" with "example"
let newStr: string = str.replace("sample", "example");

// Output the new string
console.log(newStr); // Output: Hello world! This is a example string for demonstration purposes.
```

As seen in the above code, the `replace()` method replaces the first occurrence of the specified text. To replace all occurrences, use a regular expression with the `g` (global) flag.

```
// Define a string with multiple occurrences of the word "replace"
let str: string = "Replace all replace occurrences in this string!";

// Use a regular expression to replace all occurrences of "replace" with "upgrade"
let newStr: string = str.replace(/replace/g, "upgrade");

// Output the new string
console.log(newStr); // Output: Upgrade all upgrade occurrences in this string!
```

## Deep Dive:
The `replace()` method was first introduced in JavaScript and has been implemented in TypeScript as well. It is a string method and can be used on any string variable. Other alternatives for searching and replacing text in TypeScript include the `search()` and `indexOf()` methods.

The `search()` method returns the index of the first occurrence of a specified text in a string. It takes in a regular expression as its parameter. The `indexOf()` method, on the other hand, returns the index of the first occurrence of a specified string in a string. It takes in a string as its parameter.

All three methods (`replace()`, `search()`, and `indexOf()`) can be used for searching and replacing text, but the `replace()` method is the only one that actually replaces the text.

When implementing the `replace()` method with regular expressions, the `replace()` method uses the first capturing group (parentheses in the regex) as the replacement text. If there is no capturing group, the entire match is used as the replacement.

## See Also:
- [MDN Web Docs - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Docs - String Methods](https://www.typescriptlang.org/docs/handbook/strings.html#built-in-string-methods)