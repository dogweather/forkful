---
title:    "TypeScript recipe: Deleting characters matching a pattern"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

There are many reasons why someone may want to delete characters matching a specific pattern in their TypeScript code. It can improve the readability, maintainability, and performance of the code. Additionally, deleting unnecessary characters can reduce the overall file size, making the code easier to manage.

## How To

To delete characters matching a pattern in TypeScript, we can use the `replace()` method. This method takes in a regular expression as its first argument, and a replacement string as its second argument. The regular expression can be used to match the specific pattern of characters we want to delete, while the replacement string can be left empty to replace the matched characters with nothing.

Let's take a look at an example where we want to delete all vowels from a string:

```TypeScript
let str = "Hello World!";
let newStr = str.replace(/[aeiou]/gi, "");
console.log(newStr); // Output: Hll Wrld!
```

In this code, we first declare a string variable `str` with the value "Hello World!". Then, we use the `replace()` method to replace all vowels (both lowercase and uppercase) with an empty string, effectively deleting them from the string. The `i` and `g` flags in the regular expression represent case-insensitive and global matching, respectively.

We can also use the `replace()` method to delete multiple patterns at once. For example, if we want to delete all numbers and special characters from a string, we can use the regular expression `/[0-9!@#$%^&*()]/g` as our first argument.

## Deep Dive

When using the `replace()` method in TypeScript, it's important to note that the original string is not modified. Instead, the method returns a new string with the replacements made. This means we need to assign the result to a new variable or the original variable if we want to use the modified string.

Another important thing to consider is that the `replace()` method only deletes characters matching the specified pattern. It will not remove any extra whitespace or formatting from the code. If we want to clean up our code further, we can also use the `trim()` method to remove any extra whitespace at the beginning and end of the string.

## See Also

To learn more about the `replace()` method and regular expressions in TypeScript, check out these resources:

- [MDN Web Docs - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript - Regular Expressions](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)