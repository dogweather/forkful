---
title:    "TypeScript recipe: Searching and replacing text"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why

Searching and replacing text is an essential task in programming that allows us to make changes to our code quickly and efficiently. Whether you need to replace a variable name or update a string, knowing how to search and replace text can save you time and effort in the long run.

## How To

To search and replace text in TypeScript, we can use the `replace()` method. This method takes in two parameters: the text we want to replace and the replacement text. Let's say we want to replace all instances of "cat" with "dog" in a string. We can do this by using the `replace()` method as shown below:

```TypeScript
let text = "I have a cat, a black cat."
let newText = text.replace("cat", "dog");
console.log(newText); // Output: I have a dog, a black dog.
```

We can also use regular expressions in the `replace()` method for more complex replacements. For example, if we want to replace the first occurrence of "cat" with "dog" in a string, we can use a regular expression with the `replace()` method like this:

```TypeScript
let text = "I have a cat, a black cat."
let newText = text.replace(/cat/i, "dog");
console.log(newText); // Output: I have a dog, a black cat.
```

The `/i` after the regular expression indicates that the search should be case-insensitive.

## Deep Dive

The `replace()` method in TypeScript returns a new string with the replaced text, leaving the original string unchanged. However, if we want to modify the original string, we can use the `replace()` method with a function instead of a string as the second parameter. This function takes in the matched substring as the first argument and allows us to manipulate it and return the replacement text.

For example, let's say we want to replace all occurrences of "cat" with "CAT" in a string, but only if it is followed by a comma. We can use the `replace()` method with a function to achieve this:

```TypeScript
let text = "I have a cat, a black cat."
let newText = text.replace(/cat(?=,)/g, match => match.toUpperCase());
console.log(newText); // Output: I have a CAT, a black CAT.
```

In this example, we used a regular expression with a positive lookahead `(?=,)` to match "cat" only if it is followed by a comma. The function inside the `replace()` method takes in the matched substring (in this case, "cat") and uses the `toUpperCase()` method to change it to "CAT".

## See Also

For more information on the `replace()` method and regular expressions in TypeScript, check out the following resources:

- [TypeScript Strings - replace() method](https://www.tutorialspoint.com/typescript/typescript_strings.htm)
- [JavaScript replace() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Regular Expressions in TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)