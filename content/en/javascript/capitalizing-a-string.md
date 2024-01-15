---
title:                "Capitalizing a string"
html_title:           "Javascript recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string may seem like a trivial task in programming, but it can actually have a big impact on the user experience. By capitalizing the first letter of a string, we can improve the readability and professionalism of our application, making it easier for users to understand and navigate.

## How To

To capitalize a string in Javascript, we can use the `toUpperCase()` method. This method takes in a string as a parameter and returns the same string with all uppercase letters. Let's take a look at an example:

```Javascript
let string = "hello world";
let capitalizedString = string.toUpperCase();
console.log(capitalizedString); // Output: HELLO WORLD
```

We can also use the `charAt()` method to specifically target the first letter of the string and capitalize it:

```Javascript
let string = "hello world";
let capitalizedString = string.charAt(0).toUpperCase() + string.slice(1);
console.log(capitalizedString); // Output: Hello world
```

Notice how we used the `slice()` method to extract the remaining letters of the string after capitalizing the first letter.

## Deep Dive

In Javascript, strings are immutable, meaning they cannot be changed. That's why when using the `toUpperCase()` method, a new string is created instead of modifying the original one. Additionally, the `toUpperCase()` method only capitalizes single characters, not whole words.

One way to capitalize whole words is by converting the string to an array, capitalizing each word, and then joining them back together. Let's take a look:

```Javascript
let string = "hello world";
let wordsArray = string.split(" ");
for (let i = 0; i < wordsArray.length; i++) {
  wordsArray[i] = wordsArray[i].charAt(0).toUpperCase() + wordsArray[i].slice(1);
}
let capitalizedString = wordsArray.join(" ");
console.log(capitalizedString); // Output: Hello World
```

By using this method, we can also capitalize multiple words in the string, not just the first one.

## See Also

- [MDN Web Docs: String toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN Web Docs: String charAt()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [MDN Web Docs: String split()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [MDN Web Docs: String join()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/join)