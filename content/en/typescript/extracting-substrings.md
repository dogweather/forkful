---
title:    "TypeScript recipe: Extracting substrings"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why Extract Substrings in TypeScript?

In TypeScript, strings are a common data type used for storing textual information. However, sometimes we only need a part of a string instead of the entire thing. This is where extracting substrings comes in handy. By extracting substrings, we can manipulate and use specific parts of a string, ultimately making our code more efficient and versatile.

## How To Extract Substrings in TypeScript

To extract substrings in TypeScript, we can use the built-in `substring` method. This method takes in two parameters: a starting index and an optional ending index. Let's look at an example below:

```TypeScript
let myString: string = "Hello world!";
let mySubstring: string = myString.substring(2, 7);

console.log(mySubstring); // Output: llo w
```

In this example, we used the `substring` method to extract the substring "llo w" from the original string "Hello world!" by specifying the starting index as 2 and the ending index as 7. It's important to note that the ending index is not inclusive, so the character at index 7 is not included in the extracted substring.

We can also omit the second parameter to extract the substring from the starting index to the end of the string, like this:

```TypeScript
let myString: string = "Hello world!";
let mySubstring: string = myString.substring(2);

console.log(mySubstring); // Output: llo world!
```

## Deep Dive into Substring Extraction

There are a few things to keep in mind when using the `substring` method in TypeScript. First, the starting index cannot be greater than the ending index, or else an empty string will be returned. Second, if any of the parameters are negative, they will be converted to 0.

Additionally, the `substring` method does not modify the original string but instead returns a new string with the extracted substring. This means we can store the extracted substring in a new variable or use it directly without affecting the original string.

Another thing to note is that the `substring` method is not limited to just extracting substrings from the front of a string. We can also use negative numbers for the parameters to extract substrings from the back of a string, like this:

```TypeScript
let myString: string = "Hello world!";
let mySubstring: string = myString.substring(-3, -1);

console.log(mySubstring); // Output: ld
```

## See Also
- [TypeScript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
- [Official TypeScript Documentation](https://www.typescriptlang.org/docs/handbook/2/classes.html#substring)

By extracting substrings in TypeScript, we can manipulate and use specific parts of a string easily and efficiently. So next time you need to work with just a portion of a string, remember to use the `substring` method!