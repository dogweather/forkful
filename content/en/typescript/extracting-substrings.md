---
title:    "TypeScript recipe: Extracting substrings"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# Why Extracting Substrings in TypeScript Is Useful

When working with strings in TypeScript, there are often instances where we need to extract a specific part of the string. This could be to manipulate the substring or use it in a different context. In this blog post, we will discuss why extracting substrings is a useful skill to have in your TypeScript programming toolkit.

## How To Extract Substrings in TypeScript

To extract substrings in TypeScript, we can use the `substring()` method. This method takes in two parameters: the starting index and the ending index of the substring we want to extract. Let's take a look at an example:

```TypeScript
const originalStr = "Hello world!";
const extractedStr = originalStr.substring(6, 11);
console.log(extractedStr); // output: "world"
```
In this example, we are extracting the characters from index 6 to index 11 from the original string. The extracted substring is then stored in the `extractedStr` variable and printed to the console.

We can also use the `slice()` method in a similar way to extract substrings. The difference between the `substring()` and `slice()` methods is that `substring()` does not allow negative index values, while `slice()` does. Here's an example of using `slice()` to extract a substring from the end of a string:

```TypeScript
const originalStr = "Hello world!";
const extractedStr = originalStr.slice(-6);
console.log(extractedStr); // output: "world!"
```

## Deep Dive into Extracting Substrings

When using the `substring()` method, keep in mind that the ending index is not included in the extracted substring. This means that if we want to extract a substring that includes the character at the ending index, we need to add 1 to the ending index. For example:

```TypeScript
const originalStr = "Hello world!";
const extractedStr = originalStr.substring(6, 12);
console.log(extractedStr); // output: "world!"
```

Additionally, both `substring()` and `slice()` can take in negative index values. In this case, the index is counted from the end of the string. For instance:

```TypeScript
const originalStr = "Hello world!";
const extractedStr1 = originalStr.substring(-5, -1);
const extractedStr2 = originalStr.slice(-6, -1);
console.log(extractedStr1); // output: "world"
console.log(extractedStr2); // output: "world"
```

## See Also

- [TypeScript documentation on substring method](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#working-with-strings)
- [Mozilla Developer Network article on string manipulation methods](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String#escape_notation)
- [W3Schools tutorial on working with strings in TypeScript](https://www.w3schools.com/jsref/jsref_obj_string.asp)