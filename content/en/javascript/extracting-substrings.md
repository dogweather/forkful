---
title:                "Javascript recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to extract a specific part of a string in your Javascript code? Maybe you just needed the first few characters or wanted to isolate a certain word. Whatever the reason, knowing how to extract substrings can be a useful skill for any Javascript programmer.

## How To

To extract a substring in Javascript, we can use the `slice()` method. This method takes two parameters - the starting index and the ending index of the substring we want to extract. For example:

```Javascript
let str = "Hello World";
let substring = str.slice(0, 5);

console.log(substring); // Output: Hello
```

In the above example, we used `slice()` to extract the first 5 characters of the string "Hello World" and stored it in the `substring` variable.

We can also use negative numbers as the parameters for `slice()`, which count from the end of the string. For instance:

```Javascript
let str = "Javascript is awesome";
let substring = str.slice(-7);

console.log(substring); // Output: awesome
```

In this case, we extracted the last 7 characters of the string "Javascript is awesome" and stored it in the `substring` variable.

## Deep Dive

Behind the scenes, the `slice()` method uses the string's index to determine which characters to extract. The first character of a string has an index of 0, the second character has an index of 1, and so on. This helps us understand why in the first example, we used an ending index of 5 to extract the first 5 characters - because the starting index counts as the first character.

Additionally, if the ending index is not specified, the `slice()` method will extract all characters from the starting index to the end of the string. For example:

```Javascript
let str = "Apple";
let substring = str.slice(2);

console.log(substring); // Output: ple
```

In this case, we only specified a starting index of 2, so the `slice()` method extracted all characters from index 2 until the end of the string.

## See Also

- [String.prototype.slice() - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [Substring Extracting in Javascript - W3Schools](https://www.w3schools.com/jsref/jsref_slice_string.asp)