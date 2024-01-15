---
title:                "Searching and replacing text"
html_title:           "TypeScript recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why 

Have you ever found yourself manually searching and replacing text in a large codebase? Not only is it time-consuming, but it's also prone to human error. In the world of programming, efficiency and accuracy are key, which is why learning how to properly search and replace text is essential.

## How To 

To search and replace text in TypeScript, we use the `replace()` method. This method takes in two parameters: the text to be replaced and the new text that will replace it. Let's see an example:

```TypeScript
let text = "Hello TypeScript!";
let newText = text.replace("TypeScript", "World");
console.log(newText);
```

This code will output: `Hello World!` as the new text replaces the old text. We can also use regular expressions to specify patterns to search for. For example:

```TypeScript
let text = "The brown fox jumped over the lazy dog";
let newText = text.replace(/brown|lazy/g, "quick");
console.log(newText);
```

This code will output: `The quick fox jumped over the quick dog` as it replaces both "brown" and "lazy" with "quick."

## Deep Dive 

In TypeScript, the `replace()` method also accepts a function as the second parameter. This function takes in the matched substring as its parameter and returns the replacement string. This allows for more complex replacements, such as changing the case of certain letters or performing other operations on the matched substring.

Additionally, the `replace()` method can take in regular expression flags as a third parameter. These flags allow for more precise matching, such as ignoring case or replacing all instances of the pattern.

It's important to note that the `replace()` method does not modify the original string, but instead returns a new string with the replacements made.

## See Also 

- `replace()` method in the TypeScript documentation: https://www.typescriptlang.org/docs/handbook/basic-types.html#string-replace
- Regular Expressions in TypeScript: https://www.typescriptlang.org/docs/handbook/regular-expressions.html 
- Useful string manipulation methods in TypeScript: https://blog.jscrambler.com/useful-string-manipulation-methods-in-typescript/