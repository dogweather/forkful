---
title:                "TypeScript recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why
Extracting substrings is a fundamental operation in string manipulation that allows us to isolate specific parts of a larger string. This can be useful in many scenarios such as parsing user input, formatting data, or generating dynamic strings.

## How To
To extract substrings in TypeScript, we can use the `substring()` method available on strings. This method takes in two parameters - the starting index and the ending index of the substring. Let's look at an example:

```TypeScript
let str: string = "Hello World";
let newStr: string = str.substring(0, 5);

console.log(newStr); // Outputs "Hello"
```

In this example, we are extracting the substring starting from index 0 and ending at index 5. This results in a new string containing the characters "Hello" from the original string.

We can also use negative numbers to extract substrings from the end of the string. For example, if we use `-1` as the ending index, we will get the last character of the string. Let's see this in action:

```TypeScript
let str: string = "Hello World";
let newStr: string = str.substring(6, -1);

console.log(newStr); // Outputs "World"
```

Notice how we are starting from index 6 and going backwards to get the characters "World" from the end of the string.

It's important to note that the `substring()` method does not modify the original string, but instead returns a new string with the extracted substring.

## Deep Dive
Under the hood, the `substring()` method in TypeScript uses the same logic as its JavaScript counterpart. It first checks the indices passed in and converts them to whole numbers. Then, it compares the two indices to determine which one is the starting index and which one is the ending index. If the starting index is greater than the ending index, the two indices are swapped to ensure the correct substring is extracted.

Additionally, if only one parameter is passed in, the method assumes the starting index is 0 and extracts the substring from the beginning of the string.

## See Also
- [substring() method - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [String manipulation in TypeScript - TypeScript Deep Dive](https://basarat.gitbook.io/typescript/string-handling)
- [Understanding Substrings in JavaScript - Codecademy](https://www.codecademy.com/learn/introduction-to-javascript/modules/learn-javascript-strings/cheatsheet)