---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Deleting Characters Matching a Pattern in JavaScript

## What & Why?
Deleting characters matching a pattern in JavaScript utilizes regular expression to identify and eliminate specific sequences of characters within a string. Programmers do this for data cleaning, formatting and efficient manipulation of text data. 

## How to:
Let's write some code. Consider we have a string and we want to get rid of all numbers.

```Javascript
let str = "Hello123 World456!";
str = str.replace(/\d+/g, '');
console.log(str); // Outputs: "Hello World!"
```
Here, `/\d+/g` is a regular expression wherein `\d+` matches one or more digits and `g` is a global flag to replace all matching occurrences. 

If you wish to delete all symbols but letters and numbers, you can use the \W metacharacter in combination with g (global).

```Javascript
let str = "Hello, World!";
str = str.replace(/\W/g, '');
console.log(str); // Outputs: "HelloWorld"
```

## Deep Dive
The `replace()` method was introduced as part of ECMAScript 3 (ES3) back in 1999, showing its longstanding usage in JavaScript. 

Alternatives to `replace()` include `split().join()`. This must be used wisely as it's slower for replacing instances in large strings. 

```Javascript
let str = "Hello123 World456!";
str = str.split("123").join('');
console.log(str); // Outputs: "Hello World456!"
```

It's important to note that if no global flag (g) is added, `replace()` only replaces the first occurrence.

## See Also
- [Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace) documentation on Javascript String replace() method. 
- Regular expressions tutorial and playground at [regex101](https://regex101.com/).