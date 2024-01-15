---
title:                "Capitalizing a string"
html_title:           "TypeScript recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

You may find yourself needing to capitalize a string in your TypeScript code for various reasons - whether it's for proper formatting or for user readability. Luckily, there are easy ways to do so using built-in methods in TypeScript.

## How To

To capitalize a string, you can use the `toUpperCase()` method. This will convert all characters in a string to uppercase letters. Let's take a look at an example:

```TypeScript
let myString = "hello world";
console.log(myString.toUpperCase());
```

The output of this code will be `HELLO WORLD`. We used the `toUpperCase()` method on the `myString` variable, which converted all characters in the string to uppercase letters.

If you need to capitalize only the first letter of a string, you can use the `charAt()` and `toUpperCase()` methods in conjunction. Here's an example:

```TypeScript
let myString = "hello world";
let firstLetter = myString.charAt(0).toUpperCase();
let remainingLetters = myString.slice(1);
console.log(firstLetter + remainingLetters);
```

The output of this code will be `Hello world`. We used the `charAt()` method to get the first letter of the string, then used the `toUpperCase()` method to convert it to uppercase. Then, we used the `slice()` method to get the remaining letters in the string and combined them with the first letter to get the desired output.

## Deep Dive

There are also other methods and techniques you can use to capitalize a string in TypeScript. For example, you can use regular expressions to replace the first letter of a string with its uppercase equivalent:

```TypeScript
let myString = "hello world";
let capitalizedString = myString.replace(/\b[a-z]/g, (x) => x.toUpperCase());
console.log(capitalizedString);
```

The output of this code will also be `Hello world`. We used the `replace()` method along with a regular expression to find the first letter of the string and replace it with its uppercase version.

You can also use the `charAt()` method to get the first letter of a string, convert it to its uppercase equivalent, and then use the `substr()` method to get the remaining letters in the string and concatenate them together:

```TypeScript
let myString = "hello world";
let capitalizedString = myString.charAt(0).toUpperCase() + myString.substr(1);
console.log(capitalizedString);
```

The output of this code will also be `Hello world`. We used the `charAt()` method to get the first letter of the string, converted it to uppercase, and then used the `substr()` method to get the remaining letters and combine them with the first letter to get the desired output.

## See Also

* [TypeScript Official Documentation](https://www.typescriptlang.org/docs/)
* [MDN Web Docs - String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
* [MDN Web Docs - String.prototype.charAt()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
* [MDN Web Docs - String.prototype.slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
* [MDN Web Docs - String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
* [MDN Web Docs - String.prototype.substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)