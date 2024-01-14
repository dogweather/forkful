---
title:    "Javascript recipe: Capitalizing a string"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever wanted to capitalize a string in your Javascript code? Maybe you want to display a name with proper capitalization, or format a title in a specific way. Whatever the reason, knowing how to capitalize a string can be a useful skill for any Javascript developer.

## How To

To capitalize a string in Javascript, you will need to use a combination of string methods. Let's take a look at some code examples to see how this can be done.

```Javascript
// First, we need to declare a string variable
let myString = "hello world";

// Using the .toUpperCase() method, we can convert the entire string to uppercase
let capitalizedString = myString.toUpperCase();
// Output: HELLO WORLD

// But what if we only want the first letter of the string to be capitalized?
// We can use the .charAt() method to select the first character and then the .toUpperCase() method to capitalize it
let firstLetter = myString.charAt(0).toUpperCase();
// Now we can use the .slice() method to select the rest of the string and concatenate it with the capitalized first letter
let capitalizedString = firstLetter + myString.slice(1);
// Output: Hello world
```

It's also important to note that the .toUpperCase() and .charAt() methods are case-sensitive, so be sure to double check your letters!

## Deep Dive

There are a few other methods that can be used to capitalize a string in Javascript. The .replace() method, for example, can be used to find a specific word or letter in a string and replace it with another. We can use this to capitalize the first letter of each word in a string.

```Javascript
let sentence = "this is a sentence";

let capitalizedSentence = sentence.replace(/\b\w/g, function(l){ return l.toUpperCase() });
// Output: This Is A Sentence
```

The regex expression `\b\w` means to target the beginning of each word, and the function within the replace method returns the letter with an uppercase version.

## See Also

Here are some additional resources to learn more about string manipulation in Javascript:

- [MDN web docs - String methods](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String#Methods)
- [W3Schools - Javascript Strings](https://www.w3schools.com/js/js_strings.asp)
- [FreeCodeCamp - Intro to Basic Javascript: Manipulate Arrays with .pop()](https://www.freecodecamp.org/learn/javascript-algorithms-and-data-structures/basic-javascript/manipulate-arrays-with-pop)