---
title:                "Using regular expressions"
html_title:           "Javascript recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Using regular expressions in Javascript is the process of matching patterns within a string of characters. This allows programmers to perform complex operations on strings, such as searching, replacing, and validating data. Regular expressions are especially useful when dealing with large amounts of data or when needing to perform repetitive tasks.

## How to:

To use regular expressions in Javascript, you can either create a regular expression object or use the RegExp class. Let's take a look at some examples:

### Matching a Pattern

To match a specific pattern within a string, use the `match()` method and pass in the regular expression as an argument. For example:

```Javascript
const text = "Hello world!";
const regex = /world/;
const result = text.match(regex);
console.log(result); // Output: ["world"]
```

Notice how we used the forward slashes (/) to define the pattern we want to match.

### Searching for a Pattern

To search for a specific pattern within a string, use the `search()` method and pass in the regular expression as an argument. For example:

```Javascript
const text = "Regular expressions are powerful";
const regex = /powerful/;
const result = text.search(regex);
console.log(result); // Output: 22 (index of the first match)
```

### Replacing a Pattern

To replace a specific pattern within a string, use the `replace()` method and pass in the regular expression as the first argument and the replacement string as the second argument. For example:

```Javascript
const text = "Hello, my name is John";
const regex = /John/;
const replacement = "Jane";
const result = text.replace(regex, replacement);
console.log(result); // Output: "Hello, my name is Jane"
```

## Deep Dive

Regular expressions have been around for a long time and are used in many programming languages, including Javascript. They were first introduced in the 1950s by mathematician Stephen Cole Kleene, who developed a notation for describing patterns in symbols. Regular expressions have evolved over time and have become a crucial tool for data manipulation and string processing.

There are a few alternatives to regular expressions in Javascript, such as string methods like `indexOf()`, `includes()`, and `replace()`. However, regular expressions offer more flexibility and power when it comes to performing complex string operations.

In terms of implementation, regular expressions in Javascript are created using the `RegExp()` constructor function or by using literal notation (i.e. enclosing the pattern in forward slashes). Some common modifiers that can be added to regular expressions include `i` (ignore case), `g` (global match), and `m` (multiline match).

## See Also

If you want to learn more about regular expressions in Javascript, check out the following resources:

- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Codecademy: Regular Expressions Cheatsheet](https://www.codecademy.com/learn/learn-regular-expressions/modules/learn-regular-expressions/cheatsheet)
- [Regular Expressions 101: Test and Debug Javascript Regular Expressions](https://regex101.com/)