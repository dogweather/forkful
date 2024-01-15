---
title:                "Extracting substrings"
html_title:           "Javascript recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to extract a specific section of text from a longer string in your Javascript code? Maybe you only need the first few characters, or maybe you need everything after a certain word. That's where extracting substrings comes in handy. It allows you to manipulate and retrieve specific sections of text, saving you time and effort when working with strings.

## How To

To extract a substring in Javascript, you can use the `substring()` method. This method takes in two parameters: the starting index and the ending index of the substring you want to extract. Let's take a look at an example:

```Javascript
let str = "Hello World!";
let sub = str.substring(0, 5);
console.log(sub); // Output: Hello
```
In this example, we are declaring the string "Hello World!" and using the `substring()` method to extract the first 5 characters. The starting index is 0, which is the first character of the original string, and the ending index is 5, which is the character right before the 5th index (remember, counting starts at 0). The result is the substring "Hello".

You can also use negative numbers as the parameters, which will count from the end of the string. For example:

```Javascript
let str = "Hello World!";
let sub = str.substring(6, -1);
console.log(sub); // Output: World
```

In this case, the starting index is 6, which is the character right after the space, and the ending index is -1, which is the last character of the original string.

But what if you only want to extract a portion of the end of a string without knowing the exact length? That's where the `slice()` method comes in. This method takes in one parameter, the starting index, and extracts everything from that index to the end of the string. Let's see an example:

```Javascript
let str = "Hello World!";
let sub = str.slice(6);
console.log(sub); // Output: World!
```

In this example, we are extracting everything from the 6th index (the "W") to the end of the string.

## Deep Dive

The `substring()` and `slice()` methods may seem similar at first, but they actually have some important differences. The biggest difference is that the `slice()` method can also take in a negative number as the starting index, just like we showed in the second coding example. This is because `slice()` uses the end of the string as the default value for the ending index if none is provided.

On the other hand, the `substring()` method will automatically swap the values of the parameters if the starting index is greater than the ending index. So if we were to use `str.substring(6, 0)`, it would interpret it as `str.substring(0, 6)`.

It's also important to note that both methods don't change the original string, but instead return a new string with the extracted substring. So if you want to save the result, you need to assign it to a variable like we did in our examples.

## See Also

- [MDN Web Docs: String.prototype.substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN Web Docs: String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)