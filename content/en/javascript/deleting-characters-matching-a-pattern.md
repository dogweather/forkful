---
title:    "Javascript recipe: Deleting characters matching a pattern"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered a situation where you needed to delete specific characters from a string in your JavaScript code? Maybe you needed to remove all vowels from a word, or get rid of all punctuation marks. Well, the good news is that there is a simple solution to this problem - deleting characters matching a pattern. Whether you want to clean up user input or manipulate strings for a specific purpose, this can be a useful tool in your JavaScript arsenal.

## How To

To delete characters matching a pattern in JavaScript, we will be using the `replace()` method. This method takes in two parameters, the first being the pattern we want to replace and the second being the replacement value. Let's take a look at the syntax for this method:

```Javascript
str.replace(pattern, replacement);
```

In this example, `str` represents the string we want to manipulate. The `pattern` can be a regular expression or a string containing the characters we want to remove. And, the `replacement` is the value we want to replace the characters with, which in our case will be an empty string.

Let's see this in action with an example. Say we have the string "Hello, world!" and we want to remove all punctuation marks from it. We can use the `replace()` method with a regular expression pattern to achieve this:

```Javascript
let str = "Hello, world!";
let newStr = str.replace(/[.,\/#!$%\^&\*;:{}=\-_`~()]/g, "");
console.log(newStr); // Output: Hello world
```

In this example, we used a regular expression to match all punctuation marks and replaced them with an empty string, effectively deleting them from the original string. You can also use a string as the pattern instead of a regular expression, which would replace all occurrences of that string in the string. For example:

```Javascript
let str = "Hello, my name is John.";
let newStr = str.replace("John", "");
console.log(newStr); // Output: Hello, my name is .
```

## Deep Dive

The `replace()` method is a powerful tool for manipulating strings in JavaScript, and it can do much more than just deleting characters. Let's take a closer look at the regular expression pattern we used in our example above - `/[.,\/#!$%\^&\*;:{}=\-_`~()]/g`.

First, the `g` at the end of the pattern stands for "global," which means that the method will replace all occurrences of the pattern in the string, not just the first one.

The brackets `[]` represent a character set, which means that the pattern will match any of the characters inside the brackets. In our example, it matched all punctuation marks because we included them inside the character set.

The backslash `\` is used before certain characters to escape their special meaning and treat them as regular characters. For instance, the `.` character normally means "any character" in regular expressions, but by escaping it with a backslash, we are specifying that we want to match an actual period.

There are many other special characters and modifiers that can be used in regular expressions to create complex patterns for matching and replacing text. If you're interested in learning more about regular expressions, check out [this article](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) from MDN.

## See Also

- [MDN - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [JavaScript.info - Regular Expressions](https://javascript.info/regular-expressions)