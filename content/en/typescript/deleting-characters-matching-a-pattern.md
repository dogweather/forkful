---
title:                "TypeScript recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Have you ever had a situation where you needed to delete certain characters in a string that matched a specific pattern? Perhaps you were working with user input and needed to sanitize it before using it in your code. Or maybe you were parsing through a large dataset and needed to remove unnecessary characters. Whatever the case may be, being able to delete characters matching a pattern can be a useful tool in your programming arsenal.

## How To

In TypeScript, there are a few different ways to delete characters matching a pattern. One method is to use the `replace` method on a string, passing in a regular expression as the first argument and an empty string as the second argument.

```TypeScript
const input = "Hello123 World";
const output = input.replace(/[0-9]/g, ""); 
// Output: "Hello World"
```

In this example, we are using the `/[0-9]/g` regular expression to match all numbers in the string and replacing them with an empty string. This effectively deletes all numbers from the string.

Another method is to use the `filter` method on an array. This is useful if you have a string that has been split into an array of characters and you only want to keep certain characters based on a pattern.

```TypeScript
const input = "Hello123 World".split("");
const output = input.filter(character => /[a-z ]/.test(character)).join("");
// Output: "Hello World"
```

Here, we are using the `/[a-z ]/` regular expression to test each character in the array and only keep the ones that are lowercase letters or spaces. Then, we use the `join` method to turn the filtered array back into a string.

## Deep Dive

Regular expressions are an important tool in deleting characters matching a pattern. They allow you to specify a set of characters or patterns to match and can be used in combination with string methods like `replace` or `match` to manipulate strings.

One important thing to note when using regular expressions in TypeScript is to use the `g` flag at the end to perform a global search. This ensures that all instances of the pattern in the string are matched and replaced.

Another useful feature is the ability to use more advanced patterns such as character classes, quantifiers, and alternations. These allow you to match a wider range of characters and patterns in your string.

## See Also

Regular expressions can be a powerful tool in your programming toolkit. If you're interested in learning more, here are some resources for further reading:

- [MDN Regular Expressions Guide](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Regular Expressions Documentation](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Regex101](https://regex101.com/): A helpful tool for testing and building regular expressions.