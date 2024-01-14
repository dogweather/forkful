---
title:                "Javascript recipe: Finding the length of a string"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to find the length of a string in your Javascript code? Maybe you want to validate user input or manipulate a string in a certain way. No matter the reason, knowing how to find the length of a string is an essential skill for any Javascript programmer.

## How To

Finding the length of a string in Javascript is fairly straightforward. You can use the built-in `length` property of a string to get the number of characters in it. Let's take a look at an example:

```Javascript
var str = "Hello World";
console.log(str.length);
// Output: 11
```

In this example, we declare a variable `str` and assign it the value of "Hello World". Then, we use the `length` property to get the length of the string and print it to the console. As you can see, the output is `11` because there are 11 characters in the string.

You can also use the `length` property on an empty string, which will return `0` as the output. This is because an empty string has no characters.

```Javascript
var emptyStr = "";
console.log(emptyStr.length);
// Output: 0
```

It's important to note that the `length` property counts all characters, including spaces and punctuation marks. It does not just count the letters in a string.

Now, what if you want to find the length of a string stored in a variable? You can still use the `length` property, as shown in the following example:

```Javascript
var name = "John";
console.log(name.length);
// Output: 4
```

You can also use the `length` property with string literals, as shown in the first example.

## Deep Dive

Behind the scenes, the `length` property works by accessing the `length` property of the string's internal `String` object. This object contains various methods and properties for working with strings.

Additionally, the `length` property is read-only, which means you cannot change its value. This is because the `String` object is immutable in Javascript, meaning it cannot be changed after it has been created.

It's also worth mentioning that the `length` property is not just limited to strings. It can also be used on arrays, which will return the number of elements in the array. Just like with strings, the `length` property for arrays is read-only.

## See Also

- [MDN web docs - String length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [W3Schools - Javascript string length](https://www.w3schools.com/jsref/jsref_length_string.asp)
- [JavaScript.info - Strings](https://javascript.info/string)