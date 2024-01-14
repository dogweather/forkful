---
title:    "Javascript recipe: Searching and replacing text"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

When working with large amounts of text in a JavaScript program, you may come across the need to replace specific words or phrases. This could be for formatting purposes, data cleaning, or making dynamic changes to your text. By knowing how to effectively search and replace text in JavaScript, you can save time and streamline your workflow.

## How To

In JavaScript, you can use the `.replace()` method to search and replace text in a string. This method takes two parameters: the string or regular expression to search for, and the replacement value. Let's look at an example:

```javascript
let message = "Hello, my name is John";
let newMessage = message.replace("John", "Emily");
console.log(newMessage);
```

This code would output "Hello, my name is Emily" as the newMessage, as "John" is replaced by "Emily". You can also use a regular expression as the first parameter to replace multiple occurrences of a word or phrase.

```javascript
let sentence = "I have a dog and a cat and a fish";
let newSentence = sentence.replace(/and/g, "&");
console.log(newSentence);
```

In this example, the `g` flag is added to the end of the regular expression so that all instances of "and" are replaced with "&". The output would be "I have a dog & a cat & a fish". 

## Deep Dive

The `.replace()` method also allows for a second parameter to be a function, instead of a string. This function takes in the matched string and can manipulate it before returning it as the replacement value. For example:

```javascript
let sentence = "I have $5, but I want to buy a $10 book";
let newSentence = sentence.replace(/\$\d+/g, (matched) => {
  let num = parseInt(matched[1]);
  return "$" + num * 2;
});
console.log(newSentence);
```

This code uses a regular expression to find any dollar amounts in the sentence and doubles them in the replacement value. The `\d+` matches any number of digits after the "$" and the `(matched)` parameter captures the matched string, which is then manipulated and returned as the replacement value. The output would be "I have $10, but I want to buy a $20 book".

## See Also

If you want to dive deeper into the `.replace()` method, check out the official [MDN documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace). You can also learn more about regular expressions and how to use them in JavaScript from [Regular-Expressions.info](https://www.regular-expressions.info/javascript.html). Happy coding!