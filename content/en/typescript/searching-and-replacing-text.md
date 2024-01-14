---
title:    "TypeScript recipe: Searching and replacing text"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/typescript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
The ability to search and replace text is an essential tool for any programmer. It allows you to quickly and efficiently make changes to your code without having to manually go through each line. In TypeScript, there are several ways to perform this task and it is important to understand them in order to optimize your coding experience.

## How To
To search and replace text in TypeScript, we can use the `replace()` method. This method takes two parameters - the text to be replaced and the new text to replace it with. Let's take a look at a simple example:

```TypeScript
let sentence = "I love TypeScript!";
let newSentence = sentence.replace("TypeScript", "coding");

console.log(newSentence);
```

The output of this code will be: `I love coding!` As you can see, the `replace()` method searched through the `sentence` variable and replaced the word "TypeScript" with "coding". 

But what if we want to replace multiple occurrences of a word? We can use regular expressions with the `replace()` method to do so. Regular expressions are patterns used to match character combinations in strings. Let's see this in action:

```TypeScript
let sentence = "I love TypeScript! TypeScript is the best.";
let newSentence = sentence.replace(/TypeScript/g, "coding");

console.log(newSentence);
```

The output of this code will be: `I love coding! coding is the best.` By adding the `g` modifier to our regular expression, we are telling the `replace()` method to replace all occurrences of "TypeScript" in the `sentence` variable.

## Deep Dive
In the previous examples, we used the `replace()` method directly on a string. However, we can also use it on a variable that contains a regular expression. This allows us to easily reuse the same pattern for multiple replacements. Take a look at this code:

```TypeScript
let sentence = "I love TypeScript. TypeScript is amazing.";
let regex = /TypeScript/g;
let newSentence = sentence.replace(regex, "coding");

console.log(newSentence);
```

The output will be the same as our previous example, but with the added flexibility of being able to easily change or reuse the regular expression. Additionally, the `replace()` method also accepts a function as the second parameter, allowing for even more customization in the replacement process.

## See Also
For more information on the `replace()` method and regular expressions in TypeScript, check out the following resources:

- [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [TypeScript Documentation: Regular Expressions](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [TypeScript Regular Expressions Playground](https://regex101.com/r/QNkENi/1)