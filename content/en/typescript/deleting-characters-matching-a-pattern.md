---
title:    "TypeScript recipe: Deleting characters matching a pattern"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why
Have you ever tried to clean up a string of text or code, only to realize that it contains unwanted characters that mess up your formatting? Maybe it's pesky newline characters or trailing spaces that you can't seem to get rid of. Well, fear not, because TypeScript has a simple solution for deleting characters matching a pattern.

## How To
To delete characters matching a pattern in TypeScript, we can use the `replace` method in conjunction with a regular expression. The syntax for this method is as follows:

```TypeScript
string.replace(regExp|substr, newSubStr|function)
```

We can specify the pattern we want to match as the first argument, and provide an empty string as the second argument to replace the matched characters with nothing. Let's say we have a string with some unwanted characters in it:

```TypeScript
const messyString = "Hello\nworld!"
console.log(messyString.replace(/\n/g, ''))
```

In this example, we're using a regular expression to match any newline characters (`\n`) in the string and replacing them with an empty string, effectively deleting them. The output will be: `Helloworld!`

We can also use this method to remove multiple unwanted characters or patterns at once. For example, to remove alltabs and line breaks from a string, we can use the following code:

```TypeScript
const messyString = "This is\t\t a\t\n\t\t\t\tmessy string\n\t\t"
console.log(messyString.replace(/\t|\n/g, ''))
```

The output will be: `This is a messy string`

It's important to note that the `replace` method does not mutate the original string, but rather returns a new string with the replaced characters. So if we want to save our cleaned string, we need to assign it to a variable like we did in the examples above.

## Deep Dive
Regular expressions offer a powerful and flexible way to specify patterns in strings. They use special characters and syntax to match patterns in a string, allowing for more complex pattern matching than simple string replacement.

In our examples above, we used the `g` flag in our regular expressions to indicate that we want to match the pattern globally (i.e. not stop after the first match). We can also use other flags like `i` for case-insensitive matching or `m` for multiline matching.

Regular expressions can also be combined and nested to create more specific patterns. For example, we can use character classes like `[a-z]` to match any lowercase letter, or use quantifiers like `+` to match one or more occurrences of a pattern.

A great resource for learning more about regular expressions is [RegexOne](https://regexone.com/), which offers interactive tutorials and exercises for beginners and advanced users alike.

## See Also
- [MDN Web Docs on string.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs on regular expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript Handbook on strings](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)