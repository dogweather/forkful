---
title:    "TypeScript recipe: Converting a string to lower case"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why 
Converting a string to lower case is a common task in programming, especially when dealing with user input or manipulating text data. This can be helpful in ensuring consistency and ease of comparison in your code.

## How To 
```TypeScript 
let string = "Hello World";

// Converting string to lower case 
let lowerCaseString = string.toLowerCase();
console.log(lowerCaseString); // "hello world"
```

Here, we declare a variable `string` with the value "Hello World". We then use the `toLowerCase()` method to convert the string to lower case and assign it to a new variable `lowerCaseString`. Finally, we log the new value to the console, which will output "hello world". 

Another way to achieve the same result is by using the `String.prototype.toLowerCase()` method:

```TypeScript 
let string = "Hello World";

// Converting string to lower case 
let lowerCaseString = string.toLowerCase();
console.log(lowerCaseString); // "hello world" 
```

This method works the same way as the first one, but it modifies the original string instead of creating a new variable.

## Deep Dive 
Converting a string to lower case involves changing all uppercase letters to their lowercase counterparts. This process is important because it allows for more accurate comparisons and can also help with data validation.

Some other ways to convert strings to lower case are using regular expressions or converting each character individually. However, the `toLowerCase()` method is preferred as it is more efficient and less error-prone.

It is important to note that the `toLowerCase()` method only works for ASCII characters. For non-ASCII characters, you can use the `toLocaleLowerCase()` method instead. This method also takes into account the language and locale of the string.

## See Also 
- [String.prototype.toLowerCase() method in TypeScript](https://www.typescriptlang.org/docs/handbook/2/strings.html#stringprototype-methods)
- [MDN Web Docs on converting strings to lower case](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [Comparison between `toLowerCase()` and `toLocaleLowerCase()` methods](https://stackoverflow.com/questions/2975457/what-is-the-difference-between-string-tolocalelowercase-and-string-tolower)

By now, you should have a good understanding of why and how to convert a string to lower case in TypeScript. Happy coding!