---
title:    "Javascript recipe: Extracting substrings"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Why

Substring extraction is a useful technique in Javascript that allows us to retrieve a specific portion of a string. This can be especially helpful when working with large strings or when we need to manipulate certain parts of a string without affecting the rest.

## How To

To extract a substring in Javascript, we can use the `substring()` method. This method takes two parameters, the starting index and the ending index, and returns the characters between those two indexes.

Let's take a look at an example:

```Javascript
let str = "Hello World";
let subStr = str.substring(0, 5); // "Hello"
```

In this example, we have a string `str` containing the phrase "Hello World". We use the `substring()` method to extract the first 5 characters starting from index 0, which results in the substring "Hello". 

We can also use negative indexes to start from the end of the string. For example:

```Javascript
let str = "Hello World";
let subStr = str.substring(6, -1); // "World"
```

In this case, we extract the characters starting from index 6 (the space after "Hello") and going backwards until the character before index -1 (which is the last character in the string).

It's also worth noting that the `substring()` method is non-destructive, which means it doesn't modify the original string. It simply returns the extracted substring. 

## Deep Dive

While the `substring()` method is the most commonly used way to extract substrings in Javascript, there are also other methods available such as `slice()` and `substr()`. These methods have slightly different behaviors, so it's important to understand the differences between them and choose the appropriate one for your specific use case.

Additionally, we can also use regular expressions in combination with the `match()` and `replace()` methods to extract substrings that match a certain pattern.

## See Also

Here are some helpful resources for further reading on substring extraction in Javascript:

- [MDN Web Docs - substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web Docs - slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN Web Docs - substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [MDN Web Docs - match()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/match)
- [MDN Web Docs - replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)