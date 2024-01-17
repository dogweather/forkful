---
title:                "Deleting characters matching a pattern"
html_title:           "Javascript recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern in Javascript refers to the act of removing specific characters that follow a certain sequence or rule. It is commonly done by programmers to clean up data or manipulate strings to fit a certain format.

## How to:

The following are two examples of how to delete characters matching a pattern in Javascript, using the built-in String.prototype.replace() method.

```
let string = "Hello World!";
let newString = string.replace(/[a-z]/g, ''); 

console.log(newString); // Output: H W!
```
In this first example, we use the regular expression /[a-z]/g within the replace() method to target all lowercase letters in the string and replace them with an empty string, effectively deleting them from the string.

```
let string = "123-456-789";
let newString = string.replace(/[0-9-]/g, '');

console.log(newString); // Output: Remove the numbers and dashes from the string
```
In this second example, we use the regular expression /[0-9-]/g to target all numbers and dashes in the string and replace them with an empty string. This is a useful technique for stripping out any unwanted characters from a string that may interfere with parsing or calculations.

## Deep Dive:

### Historical Context:
The concept of deleting characters matching a pattern has been around since the earliest versions of Javascript. Regular expressions were first introduced in 1995 and have been a fundamental tool for pattern matching and data manipulation ever since.

### Alternatives:
Apart from using regular expressions, there are other ways to delete characters matching a pattern in Javascript. One alternative method is to use the built-in String.prototype.substring() method, which takes in two parameters - the starting index and the ending index. This would allow you to remove certain characters within a string using their index positions.

### Implementation Details:
The String.prototype.replace() method not only allows for the replacement of characters, but it also supports the use of capturing groups and functions. Capturing groups allow you to extract specific parts of the matched string, while functions allow for more complex calculations to be carried out for each match.

## See Also:

- [MDN Web Docs on String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs on Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [W3Schools tutorial on Regular Expressions in Javascript](https://www.w3schools.com/js/js_regexp.asp)