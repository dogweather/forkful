---
title:    "Javascript recipe: Converting a string to lower case"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Why

Sometimes, in programming, we may come across a situation where we need to convert a string to lower case. This could be due to various reasons such as comparing strings without worrying about case sensitivity or just simply following coding conventions. Thankfully, in JavaScript, this task is made easier with built-in methods.

## How To

To convert a string to lower case in JavaScript, we can use the `toLowerCase()` method. Let's take a look at an example:

```Javascript
let string = "HELLO WORLD!";
let lowerCaseString = string.toLowerCase();
console.log(lowerCaseString); // Output: hello world!
```

As we can see, the `toLowerCase()` method converts all the characters in the string to lower case. We can also use the `toLocaleLowerCase()` method for a more locale-specific conversion. Here's an example:

```Javascript
let string = "İSTANBUL";
let lowerCaseString = string.toLocaleLowerCase("tr-TR");
console.log(lowerCaseString); // Output: istanbul
```

In the above example, we used the `tr-TR` locale for the Turkish language which converts the capital letter 'İ' to a lowercase 'i'.

## Deep Dive

When we use the `toLowerCase()` method, all the special characters and symbols in the string remain the same. For instance, if we have a string with a mix of upper and lower case letters, as well as special characters, the `toLowerCase()` method will only convert the letters and leave the special characters untouched. Let's see an example:

```Javascript
let string = "H@llo W0RLD!";
let lowerCaseString = string.toLowerCase();
console.log(lowerCaseString); // Output: h@llo w0rld!
```

Similarly, the `toLocaleLowerCase()` method also only converts the letters and not the special characters. 

## See Also

- [String.prototype.toLowerCase() documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [String.prototype.toLocaleLowerCase() documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase) 
- [JavaScript strings and case sensitivity](https://javascript.info/string#case-sensitivity)