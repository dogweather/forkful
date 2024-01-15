---
title:                "Converting a string to lower case"
html_title:           "Javascript recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting strings to lower case can be a useful feature in many cases. It can help to standardize input, make data more easily searchable, and improve user experience by reducing potential errors in input.

## How To

```Javascript
const str = "HELLO WORLD";

// Using the toLowerCase() method
const lowerCaseStr = str.toLowerCase();

// Output: "hello world"
console.log(lowerCaseStr);
```

The `toLowerCase()` method is a built-in function in Javascript that converts a string to lower case. It can be applied to any string variable or string literal. 

```Javascript
const str = "1234";

// Output: "1234"
console.log(str.toLowerCase());
```

Even if the string contains numbers, the `toLowerCase()` method will only convert letters to lower case and leave numbers unchanged. 

```Javascript
const str = "HELLO WORLD";

// Using a for loop
let lowerCaseStr = "";

for (let i=0; i < str.length; i++) {
    if (str[i] >= "A" && str[i] <= "Z") {
        lowerCaseStr += str[i].toLowerCase();
    } else {
        lowerCaseStr += str[i];
    }
}

// Output: "hello world"
console.log(lowerCaseStr);
```

Another way to convert a string to lower case is by iterating through each character in the string and checking if it is a letter. If it is, then we can use the `toLowerCase()` method on that specific character and add it to our new string variable. This method may be useful for situations where you need to manipulate the string further before converting it to lower case.

## Deep Dive

The `toLowerCase()` method uses the Unicode standard to convert alphabetic characters to lower case. This means that it can handle letters from different alphabets and languages, not just the English alphabet.

It is also important to note that the `toLowerCase()` method does not mutate the original string, but instead returns a new string with the converted letters. This is because strings in Javascript are immutable, meaning they cannot be changed in-place.

In addition to the `toLowerCase()` method, there is also a similar method called `toUpperCase()` which converts a string to upper case. Both methods can be useful when performing operations on strings in various projects, such as web development or data manipulation.

## See Also

- [Javascript String Reference - toLowerCase() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Unicode Character Code Charts](https://unicode.org/charts/)
- [Javascript String Reference - toUpperCase() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)

Converting strings to lower case may seem like a small detail, but it can greatly improve the quality and functionality of your code. Now that you know how to use the `toLowerCase()` method, you can confidently apply it in your own projects. Happy coding!