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

## What & Why?
Capitalizing a string in programming means changing the first letter of the string elements such as words or sentences to uppercase. Programmers do this to clean up user input data, enhance readability, or meet the grammatical or presentation needs of the software's text content.

## How to:
There is no built-in JavaScript function for string capitalization, but it's easy to do it with base JavaScript methods like `slice`, `charAt`, and `toUpperCase`.

Let's take a look at a simple way to capitalize a string:
```Javascript
function capitalizeString(str) {
    return str.charAt(0).toUpperCase() + str.slice(1);
}
console.log(capitalizeString("hello world")); // Outputs: "Hello world"
```
This function picks the first character of a string, converts it to uppercase and adds rest of the string (from position 1 to end) also known as slicing.

You can also capitalize every word in a string:
```Javascript
function capitalizeWords(str) {
    return str.split(' ').map(function(word) {
        return word.charAt(0).toUpperCase() + word.slice(1);
    }).join(' ');
}
console.log(capitalizeWords("hello world")); // Outputs: "Hello World"
```
We first split the string into words, then apply the same capitalization function to each word, and finally join them back together with a space.

## Deep Dive
The need to capitalize a string has existed since the creation of programming languages, but JavaScript doesn't provide direct support for this operation, so the developer community has found ways around this using the language's existing methods.

Among alternative methods, lodash, a popular utility library for JavaScript, provides the `_.capitalize` function. However, this only capitalizes the first letter and makes the rest lowercase. 

Meanwhile, its implementation details are quite simple: fetch the first char, make it uppercase, slice the rest of the string, and then combine them.

## See Also
For additional JavaScript string methods such as `slice`, `charAt`, and `toUpperCase`, check them out at [Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String).

For a more comprehensive string utility library, visit [lodash](https://lodash.com/) or its [_.capitalize method](https://lodash.com/docs/4.17.15#capitalize). 

Also, check out how to handle [string manipulation](https://www.freecodecamp.org/news/javascript-string-manipulation/) in a broader context for JavaScript.