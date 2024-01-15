---
title:                "Converting a string to lower case"
html_title:           "TypeScript recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Have you ever had to deal with a string that is not in the desired case? Maybe you received user inputs or data from an API that are inconsistently capitalized. In these situations, converting a string to lower case can be a useful and necessary step in order to eliminate possible errors and make the code more consistent.

## How To

To convert a string to lower case in TypeScript, you can use the built-in method `toLowerCase()` available for all string objects. This method takes no arguments and returns the string converted to lower case. Let's take a look at an example:

```TypeScript
let myString = "tHIs Is A sTRiNG";
let convertedString = myString.toLowerCase();

console.log(convertedString); // output: "this is a string"
```

In the example above, we declare a string variable `myString` with mixed cases. Then, using `toLowerCase()` method, we assign the returned value to `convertedString` variable. Finally, we log the output to the console and see that the string is now in lower case.

You can also use this method in conjunction with user inputs and data from APIs. For instance, if you are building a form that requires usernames to be in lower case, you can use `toLowerCase()` method to ensure consistency before submitting the data.

## Deep Dive

Under the hood, `toLowerCase()` method uses Unicode values to convert each character to its corresponding lower case. This means that it can handle different languages and characters, making it a reliable method for string conversions.

In addition to `toLowerCase()`, TypeScript also provides another method `toLocaleLowerCase()` which follows the language-specific case mappings defined by the runtime environment. This can be useful when dealing with multi-lingual applications.

It is important to note that `toLowerCase()` and `toLocaleLowerCase()` methods return a new string, leaving the original string untouched. If you want to change the original string, you can reassign the returned value or use `string.replace()` method.

## See Also

- [TypeScript Official Documentation on String Methods](https://www.typescriptlang.org/docs/handbook/2/types-from-types.html#string-methods)
- [MDN Web Docs on `toLowerCase()` method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)