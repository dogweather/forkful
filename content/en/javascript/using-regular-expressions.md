---
title:    "Javascript recipe: Using regular expressions"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are an essential tool for any programmer, allowing you to search, replace, and manipulate text efficiently. If you've ever been frustrated with manually searching and replacing text in a document or code, then learning regular expressions will save you time and effort.

## How To

### Basic Syntax
Regular expressions are denoted by forward slashes (`/`) and include the pattern to match and any modifiers. For example, if we wanted to extract all the words that start with the letter "s" from a string, we could use the regular expression `/s\w+/g` where the `s` literal searches for the letter "s" and `\w+` matches any word character (letters, digits, and underscores) one or more times. The `g` modifier ensures that all instances are found and not just the first one.

```Javascript
const string = "She sells seashells by the seashore.";
const regex = /s\w+/g;
const wordsStartingWithS = string.match(regex);
console.log(wordsStartingWithS); // ["sells", "seashells", "seashore"]
```

### Escaping Special Characters
Some characters have special meanings in regular expressions and need to be escaped with a backslash (`\`) if you want to match them literally. For example, if we wanted to find all the parentheses in a string, we would need to escape the opening and closing parentheses, like this: `/[\(\)]+/g`.

```Javascript
const string = "I have (probably) overused parentheses.";
const regex = /[\(\)]+/g;
const parentheses = string.match(regex);
console.log(parentheses); // ["(", ")"]
```

### Using Quantifiers
Quantifiers specify the number of times that a pattern should be matched. They are denoted by curly braces (`{}`) and are usually used after a character or group. For example, if we wanted to find all the words that are three letters long in a string, we could use the regex `/\w{3}/g`.

```Javascript
const string = "The cat sat on the mat.";
const regex = /\w{3}/g;
const threeLetterWords = string.match(regex);
console.log(threeLetterWords); // ["The", "cat", "mat"]
```

## Deep Dive

Regular expressions have many other features and modifiers that allow for more complex searches and replacements. Some popular ones include:

- `i`: ignores case sensitivity
- `m`: multi-line mode, allowing for matching on multiple lines
- `w`: matches any non-word character (opposite of `\w`)
- `^` and `$`: matches the beginning and end of a string, respectively

Regular expressions can also be used in combination with string methods, such as `replace()`, `search()`, and `split()`, to perform more targeted text manipulations.

It's important to note that regular expressions can be challenging to wrap your head around at first, but with practice and experience, they become a fundamental tool in a programmer's arsenal.

## See Also
- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Mastering Regular Expressions by Jeffrey E. F. Friedl](https://www.oreilly.com/library/view/mastering-regular-expressions/9780596528126/)