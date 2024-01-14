---
title:    "TypeScript recipe: Capitalizing a string"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Why
As developers, we often need to manipulate strings in our code. One common task is capitalizing a string, which means making the first letter uppercase. This can be useful in many situations, such as formatting user input or displaying titles.

## How To
In TypeScript, there are a few different ways to capitalize a string. Let's take a look at some examples and their output:

```
const str1: string = "hello world";
console.log(str1.charAt(0).toUpperCase() + str1.slice(1));
// Output: Hello World

const str2: string = "javascript";
console.log(str2.replace(/^\w/, (c) => c.toUpperCase()));
// Output: JavaScript
```

In the first example, we are using the `charAt()` and `slice()` methods to extract the first letter and then concatenate it with the rest of the string. In the second example, we are using the `replace()` method with a regular expression and a callback function to replace the first letter with its uppercase version.

There are also built-in TypeScript functions that can help with capitalizing a string. For instance, the `toUpperCase()` function will make all letters in the string uppercase, not just the first one. So, if we only want to capitalize the first letter, we can use the `slice()` method to get the rest of the string and concatenate it with the first letter.

```
const str3: string = "typescript";
console.log(str3.toUpperCase().charAt(0) + str3.slice(1));
// Output: TypeScript
```

Another useful function for capitalizing strings is `split()`, which splits a string into an array of substrings. We can use this in combination with the `map()` and `join()` methods to capitalize each word in a sentence.

```
const str4: string = "hello to the world";
console.log(str4.split(" ").map((word) => word.charAt(0).toUpperCase() + word.slice(1)).join(" "));
// Output: Hello To The World
```

## Deep Dive
As you can see from the examples, there are a few different approaches to capitalizing a string in TypeScript. However, it's worth noting that these methods only work if the string contains only letters. If the string contains other characters, such as numbers or symbols, the results may not be as expected.

There are also other considerations to keep in mind when capitalizing strings, such as locale-specific rules and special cases for certain words. For more complex scenarios, you may need to use a library or create a custom function to handle all cases properly.

## See Also
- [MDN Web Docs: toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN Web Docs: map()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map)
- [MDN Web Docs: join()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join)