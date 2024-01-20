---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Article: Searching and Replacing Text in JavaScript

## What & Why?

Searching and replacing text in JavaScript is all about finding specific characters, words, or patterns in a string and substitifying them with different ones. And why do we do it? It's handy for everything from cleaning up user inputs to converting data formats and more.

## How to:

The simplest way to search and replace text in JavaScript is with the `replace()` method:

```Javascript
let str = 'Hello, World!';
let newStr = str.replace('World', 'JavaScript');
console.log(newStr);
// Output: 'Hello, JavaScript!'
```

`replace()` only changes the first match it finds. To replace all occurrences of a substring, use a regular expression with the `g` (global) flag:

```Javascript
let str = 'Apple, apple, I love apple!';
let newStr = str.replace(/apple/gi, 'banana');
console.log(newStr);
// Output: 'Banana, banana, I love banana!'
```

The `i` flag makes the search case-insensitive.

## Deep Dive

In ye olden days, JavaScript didn't have the global (`g`) flag. You'd have to write a loop and call `replace()` multiple times.

Luckily for us, JavaScript now supports regular expressions. They're powerful and flexible, but can be confusing if you're a beginner. They also get complex if you're dealing with special characters or searching for whole words.

To search for whole words, you need to include the word boundaries `\b` in your regex:

```Javascript
let str = 'Assess an assessment with sass.';
let newStr = str.replace(/\bass\b/gi, 'class');
console.log(newStr);
// Output: 'Assess an assessment with class.'
```

JavaScript doesn't have a built-in method for replacing strings by index. But with substring manipulation:

```Javascript
let str = 'Hello, Friend!';
let newStr = str.substring(0, 7) + 'World' + str.substring(12);
console.log(newStr);
// Output: 'Hello, World!'
```

## See Also

If you'd like more detailed guides, MDN has great docs on [replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace) and [RegExr](https://regexr.com/) is an interactive tool to learn, build, and test regular expressions. And for the brave of heart, [JavaScript.info](https://javascript.info/regexp-introduction) goes deep into regular expressions in JavaScript, no holds barred.