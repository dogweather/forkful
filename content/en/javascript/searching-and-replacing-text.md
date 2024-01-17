---
title:                "Searching and replacing text"
html_title:           "Javascript recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is a common task in programming, which involves finding specific words or characters within a piece of text and replacing them with different ones. This is often done to make changes or corrections to large amounts of text without having to manually edit each instance. It saves time and helps ensure accuracy in the code.

## How to:

One way to search and replace text in Javascript is by using the `replace()` method. This method takes two parameters: the text to be searched and the text to replace it with. For example:

```Javascript
let str = "Hello world!";

let newStr = str.replace("world", "universe");

console.log(newStr); // output: Hello universe!
```

In the code above, the `replace()` method finds the word "world" within the string and replaces it with "universe". The `replace()` method also has another useful feature where it can accept a regular expression as the first parameter to search for a pattern instead of a specific word. For example:

```Javascript
let str = "Hello world!";

let newStr = str.replace(/world/, "universe");

console.log(newStr); // output: Hello universe!
```

## Deep Dive:

The `replace()` method has been a part of Javascript since its first edition in 1995. There are other methods in Javascript that can be used for searching and replacing text, such as `indexOf()`, `search()`, and `split()`. However, the `replace()` method is the most versatile and commonly used.

Other alternative ways to search and replace text in Javascript include using libraries such as jQuery or Lodash, which provide more advanced and efficient functions for searching and replacing text.

The `replace()` method can also accept a callback function as the second parameter, which is executed for each match found. This allows for more complex operations to be performed during the replacement process.

## See Also:

- [MDN web docs on replace() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [jQuery documentation on text manipulation](https://api.jquery.com/category/manipulation/)
- [Lodash documentation on string methods](https://lodash.com/docs/4.17.15#string)