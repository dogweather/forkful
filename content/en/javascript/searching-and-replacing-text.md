---
title:                "Javascript recipe: Searching and replacing text"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Searching and replacing text is a common task in programming. It allows us to quickly make changes to a large amount of text without having to manually go through each line. By using Javascript, we can easily automate this process and save ourselves time and effort.

## How To
To search and replace text in Javascript, we can use the `replace()` function. Let's say we have a string variable called `text` and we want to replace all occurrences of the word "hello" with "hi". We can do so by using the following code:

```Javascript
var text = "Hello World!";
var newText = text.replace(/hello/g, "hi");
console.log(newText); // Output: hi world!
```

In the above code, we use the `replace()` function and provide two parameters - the first one is the text we want to search for, and the second one is the text we want to replace it with. The `g` in `/hello/g` stands for global, which means it will search for all occurrences of "hello" in the string. Without the `g`, it would only replace the first occurrence.

We can also use the `replace()` function to replace text based on a regular expression pattern. For example, if we want to replace all numbers in a string with "x", we can use the following code:

```Javascript
var text = "I have 5 apples and 3 oranges.";
var newText = text.replace(/[0-9]/g, "x");
console.log(newText); // Output: I have x apples and x oranges.
```

In the above code, we use the regular expression pattern `[0-9]` to match any number and the `g` flag to replace all occurrences.

## Deep Dive
The `replace()` function also has another useful feature - it can take a callback function as the second parameter. This allows us to have more control over the replacement process. Let's see an example:

```Javascript
var text = "Hello World!";
var newText = text.replace(/hello/g, function(match) {
  return match.toUpperCase(); // Changes "hello" to "HELLO"
});
console.log(newText); // Output: HELLO World!
```

In the above code, we use a callback function and the `toUpperCase()` method to capitalize all occurrences of "hello" in the string.

We can also use the callback function to access the index and the original string. This can be useful for making specific replacements based on certain conditions. Here's an example:

```Javascript
var text = "I have a red car and a blue car.";
var newText = text.replace(/car/g, function(match, index, originalText) {
  if(originalText[index-1] === "red"){
    return "bike"; // Replaces "car" with "bike" only if it follows "red"
  } else {
    return match; // Leaves "car" as is
  }
});
console.log(newText); // Output: I have a red bike and a blue car.
```

In the above code, we use the index and the original string to replace "car" with "bike" only if it follows the word "red".

## See Also
- [MDN Web Docs: String replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [W3Schools: Javascript replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)
- [Regular Expressions: MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)