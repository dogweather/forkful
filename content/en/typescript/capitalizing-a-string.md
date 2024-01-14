---
title:                "TypeScript recipe: Capitalizing a string"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string may seem like a simple task, but it can actually have a significant impact on the readability and organization of your code. By converting all or part of a string to capitalized letters, you can make your code easier to understand and maintain. Additionally, some programming languages and frameworks may require strings to be capitalized in order to function correctly. In this blog post, we will explore how to capitalize a string using TypeScript, a popular and powerful superset of JavaScript.

## How To

To capitalize a string in TypeScript, we can use the `toUpperCase()` method. This method converts all characters in a string to uppercase letters. For example, if we have the string "hello world", we can use `toUpperCase()` to convert it to "HELLO WORLD". Let's see how this looks in code:

```TypeScript
let str = "hello world";
let capitalizedStr = str.toUpperCase();
console.log(capitalizedStr); // Output: "HELLO WORLD"
```

Simple, right? But what if we only want to capitalize the first letter of a string, leaving the rest as lowercase? We can achieve this using a combination of `toUpperCase()` and `slice()`, which allows us to extract a portion of a string. Take a look at the following example:

```TypeScript
let str = "hello world";
let capitalizedStr = str[0].toUpperCase() + str.slice(1);
console.log(capitalizedStr); // Output: "Hello world"
```

By using `toUpperCase()` on only the first character and then combining it with the rest of the string using `slice()`, we can effectively capitalize only the first letter. 

## Deep Dive

Now that we know how to capitalize a string in TypeScript, let's take a closer look at how this method works. The `toUpperCase()` method is actually available on the `String` prototype, meaning it can be used on any string variable. This method converts all lowercase letters in the string to their uppercase counterparts, while leaving any already uppercase letters unchanged. It also ignores any non-alphabetic characters. This allows us to use `toUpperCase()` on strings containing numbers or special characters without any issues.

We also used the `slice()` method to extract a portion of a string and combine it with the capitalized first letter. This method takes two parameters: the starting index and the ending index. By only passing in the starting index, we can effectively extract the rest of the string starting from that point. In our case, we passed in the index of `1`, since the first letter is already capitalized and we only want to modify the rest of the string.

## See Also

If you want to learn more about TypeScript or explore other programming languages, check out these helpful links:

- [Official TypeScript documentation](https://www.typescriptlang.org/docs/home.html)
- [FreeCodeCamp's TypeScript tutorial](https://www.freecodecamp.org/news/typescript-tutorial-for-beginners/)
- [Top programming languages to learn in 2021](https://www.thebalancecareers.com/top-programming-languages-2063213)