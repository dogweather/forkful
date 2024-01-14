---
title:                "TypeScript recipe: Extracting substrings"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substring extraction is an important concept in TypeScript that allows programmers to extract a specific part of a string. This can be useful for various purposes such as data manipulation or formatting. Learning how to extract substrings will make your code more efficient and versatile. 

## How To

To extract a substring in TypeScript, we use the `slice()` method. It takes in two parameters, the starting index and the ending index of the substring. For example, if we have a string called `name` with the value "John Doe", and we want to extract "John", we can do so with the following code:

```TypeScript
let name: string = "John Doe";
let firstName: string = name.slice(0,4);
console.log(firstName);
```

The output of this code will be "John". The first parameter, 0, represents the starting index (first letter) of the substring, while the second parameter, 4, represents the ending index (before the 5th letter) of the substring. Keep in mind that the ending index is not inclusive, meaning the character at that index will not be included in the substring. 

We can also use negative numbers in the `slice()` method to start counting from the end of the string. For example, if we want to extract "Doe" from our `name` string, we can do so with the following code:

```TypeScript
let lastName: string = name.slice(-3);
console.log(lastName);
```

The output of this code will be "Doe". The negative index -3 represents the last three characters of the string. If we want to extract all of the characters after a certain index, we can simply omit the second parameter. For example, if we wanted to extract "Doe" as well as all the characters after it, we can use the following code:

```TypeScript
let lastName: string = name.slice(-3);
console.log(lastName);
```

The output of this code will be "Doe".

## Deep Dive

The `slice()` method in TypeScript can take in negative values for both its parameters. This is because in TypeScript, strings are indexable in both forward and reverse directions. This makes it very flexible and allows us to easily manipulate strings in various ways. Additionally, the `slice()` method does not modify or change the original string, it simply returns the extracted substring. 

It is important to note that the `slice()` method does not support adding or replacing characters within the string. It only allows for extraction of substrings. If you want to add or replace characters, you can use the `replace()` method and combine it with the `slice()` method to achieve the desired result. 

## See Also

- [String.prototype.slice() - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [TypeScript String manipulation - W3Schools](https://www.w3schools.com/js/js_type_conversion.asp)
- [Using Template Literals in TypeScript - Medium](https://medium.com/@urish/using-template-literals-in-typescript-435583f67614)