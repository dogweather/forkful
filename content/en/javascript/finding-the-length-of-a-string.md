---
title:    "Javascript recipe: Finding the length of a string"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever wondered how to find the length of a string in Javascript? As a beginner in programming, it's important to learn the basics of manipulating strings, and finding their length is a crucial skill to have. In this blog post, we will dive deeper into why it's important to know how to find the length of a string and how to do it successfully.

## How To

Coding Example 1:
```
// Input
let string = "Hello World";

// Output
console.log(string.length);
// Output: 11
```

Coding Example 2:
```
// Input
let string = "This is a long string with multiple words and spaces";

// Output
console.log(string.length);
// Output: 47
```

As seen in the code examples above, finding the length of a string in Javascript is as simple as using the `length` property on a string variable. This property returns the number of characters in the string, including spaces and punctuation marks. It's important to note that the `length` property starts counting from 1, not 0, so if you want to access the last character of a string, you will need to subtract 1 from the length.

Finding the length of a string can come in handy when validating user input, manipulating data, or simply displaying information to the user. It's an essential skill to have when working with strings in Javascript.

## Deep Dive

Behind the scenes, the `length` property uses a method called `toString()` to convert the string to a string object and then uses the `.length` method to retrieve the number of characters. This means that the `length` property only works on string objects and not on other data types such as numbers or booleans.

It's also important to note that the `length` property only counts the characters in a string, not the words. For example, in the second coding example, the string has multiple words and spaces, but the `length` property still returns 47 as that is the number of characters in the string.

## See Also

For more information on string manipulation in Javascript, check out these helpful resources:

- [MDN web docs on string length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [W3Schools tutorial on string length](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [FreeCodeCamp article on string manipulation](https://www.freecodecamp.org/news/string-manipulation-in-javascript/)

Now that you know how to find the length of a string in Javascript, go forth and practice your new skill! Happy coding!