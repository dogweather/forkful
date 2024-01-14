---
title:                "Javascript recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever wanted to know how many characters are in a word or sentence? Maybe you're trying to set a character limit for a tweet or limit the number of characters in a username. Whatever the reason may be, finding the length of a string in Javascript can be a useful tool in your programming arsenal.

## How To

To find the length of a string in Javascript, you can use the built-in method `length()`. This method returns the number of characters in a string, including spaces and punctuation.

Let's take a look at an example:

```Javascript
let myString = "Hello World!";
console.log(myString.length()); // Output: 12
```

In this example, we have declared a variable called `myString` and assigned it the value of "Hello World!". Then, by using the `length()` method, we are able to determine that the string contains 12 characters.

Keep in mind that the index of a string starts at 0, so the last character in our string would be at index position `myString.length() - 1`.

## Deep Dive

When using the `length()` method, it is important to note that it only applies to strings. If you try to use it on any other type of data, such as a number or boolean, it will return undefined. This is because `length()` is a string method and cannot be applied to other types of data.

Another thing to keep in mind is that the `length()` method gives the total number of characters, including any escape characters. For example, if you have a string with a `"\n"` for a line break, it will count as one character in the length of the string.

Additionally, `length()` will also include any empty spaces at the beginning or end of a string. If you want to exclude these spaces, you can use the `trim()` method first and then apply `length()` to the trimmed string.

## See Also

For more information on the `length()` method and other string methods in Javascript, check out the following resources:

- [MDN Web Docs - String.prototype.length()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [W3Schools - JavaScript String Length](https://www.w3schools.com/js/js_string_length.asp)
- [JavaScript String Methods Tutorial](https://www.javascripttutorial.net/javascript-string-methods/)