---
title:                "TypeScript recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why: 

Capitalizing a string may seem like a simple task, but it can actually be quite useful in certain scenarios. For example, if you are creating a program that requires proper capitalization for names or titles, having a built-in function to do so can save time and improve the overall quality of your code. Plus, it can make your output look more professional and polished.

## How To:

To capitalize a string in TypeScript, we can use the built-in `toUpperCase()` method. This method converts all lowercase letters in a string to uppercase. Let's take a look at an example:

```TypeScript
let name: string = 'jennifer';

console.log(name.toUpperCase()); // outputs "JENNIFER"
```

In this example, we have declared a string variable `name` with the value of "jennifer". Then, we use the `toUpperCase()` method to convert all the lowercase letters in the string to uppercase, and finally, we use `console.log()` to print the new capitalized string to the console.

We can also use this method with user input. For example, if a user enters their name in all lowercase letters, we can use the `toUpperCase()` method to properly capitalize their name before displaying it.

```TypeScript
let userInput: string = prompt('Please enter your name.');

console.log(userInput.toUpperCase());
```

You can try this code out in the TypeScript playground [here](https://www.typescriptlang.org/play/index.html).

## Deep Dive:

The `toUpperCase()` method can also be used with strings that already have some uppercase letters. In this case, only the lowercase letters will be converted to uppercase, leaving the uppercase letters unchanged. For example:

```TypeScript
let title: string = 'tHe lOrd Of The rIngS';

console.log(title.toUpperCase()); // outputs "THE LORD OF THE RINGS"
```

Additionally, the `toUpperCase()` method does not alter the original string, but rather returns a new string with the changes applied. This can be useful if you need to keep the original string as it is.

It's also important to note that the `toUpperCase()` method only works on strings and will throw an error if used on any other data type. 

## See Also:

For more information on the `toUpperCase()` method and other useful string methods in TypeScript, check out the official TypeScript documentation [here](https://www.typescriptlang.org/docs/handbook/strings.html).

Other helpful resources for learning TypeScript:

- [TypeScript for Beginners: The Missing Cheat Sheet](https://www.freecodecamp.org/news/typescript-cheat-sheet/)
- [TypeScript Tutorial for Programmers](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
- [TypeScript Crash Course](https://www.youtube.com/watch?v=rAy_3SIqT-E) on YouTube