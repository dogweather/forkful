---
title:                "TypeScript recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

At some point in your programming journey, you may come across the task of searching and replacing text. Whether it's fixing small typos or making larger changes, finding and replacing specific words or phrases in your code can save you a lot of time and effort. This simple yet useful technique is definitely worth learning in TypeScript.

## How To

To demonstrate searching and replacing text in TypeScript, let's imagine we have a string that contains the word "color" and we want to replace it with "colour". Here's how we could approach this task:

```
TypeScript let sentence = "I love the color blue."
let newSentence = sentence.replace("color", "colour")
console.log(newSentence);
```

In this example, we are using the built-in `replace()` method to search for the word "color" in the `sentence` variable and replace it with "colour". This method returns a new string with the replaced text, which we can then assign to a new variable called `newSentence`. When we print `newSentence` to the console, we get the following output:

```
I love the colour blue.
```

It's that simple! The `replace()` method is case-sensitive, so make sure to use the correct spelling when searching for text to replace.

## Deep Dive

While the example above shows a basic use case for searching and replacing text in TypeScript, there are many ways to customize and refine this technique. Here are a few things to keep in mind:

- The `replace()` method only replaces the first instance of the specified text. If you want to replace all instances, you can use the global flag `/g` at the end of your search string, like this: `sentence.replace(/color/g, "colour")`.

- You can also use regular expressions to search for more complex patterns and replace them accordingly. For example, you could use a regular expression to replace all occurrences of the word "color" regardless of whether it's capitalized or not.

- In some cases, you may want to use a callback function to manipulate the replacement text. This allows for more dynamic and advanced replacements. To do this, you can add a callback function as the second argument of the `replace()` method, like this: `sentence.replace(/color/g, (match) => match.toUpperCase())`. This will replace all instances of "color" with the uppercase version of itself.

## See Also

To learn more about the `replace()` method and other string manipulation techniques in TypeScript, check out the following resources:

- [MDN Web Docs - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [TypeScript Documentation - String Operations](https://www.typescriptlang.org/docs/handbook/2/types-from-types.html#string-operations)
- [W3Schools - TypeScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)