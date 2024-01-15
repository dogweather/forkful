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

## Why

Have you ever needed to change a particular phrase or word in a large chunk of text? This is where searching and replacing text comes in handy. By using Javascript, we can easily search for specific text and replace it with something else, saving us time and effort.

## How To

To search and replace text in Javascript, we first need to declare a string variable that contains the text we want to modify:

```Javascript
let text = "I love coding in Javascript."
```

Next, we use the `replace()` method to search for a specific phrase or word and replace it with something else. In this example, let's replace "love" with "enjoy":

```Javascript
let replacedText = text.replace("love", "enjoy");
console.log(replacedText);
```

The output will be: "I enjoy coding in Javascript."

We can also use regular expressions with the `replace()` method to search and replace text, giving us even more flexibility. For example, if we want to remove all numbers from a string, we can do so by using a regular expression that matches any digits in the text:

```Javascript
let text = "I have 5 apples, 3 oranges, and 2 bananas.";
let removedNumbers = text.replace(/\d+/g, '');
console.log(removedNumbers);
```

The output will be: "I have apples, oranges, and bananas."

## Deep Dive

Under the hood, the `replace()` method uses regular expressions to perform the search and replace operation. This means we can use modifiers such as `i` to make the search case-insensitive, or `g` to replace all instances instead of just the first one.

We can also use the `replace()` method with a callback function, which gives us even more control over the replacement process. The callback function takes in the matched substring, the regex capture groups, the index in the original string, and the full string as parameters. This allows for complex replacements to be made based on the matched substring.

## See Also

Here are some resources for further reading on searching and replacing text in Javascript:

- [MDN Documentation on String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Regular Expressions Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/javascript)
- [Regex101 - Online Regular Expression Tester](https://regex101.com/)

And there you have it! With the `replace()` method and regular expressions, you can easily search and replace text in Javascript. Happy coding!