---
title:    "Javascript recipe: Searching and replacing text"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Why

Have you ever found yourself manually going through a large chunk of text, searching for a specific word or phrase to replace? Or perhaps you've noticed a spelling error that needs to be corrected throughout an entire document. These tasks are not only time-consuming, but they can also be prone to mistakes. Luckily, there is a more efficient and accurate solution - using Javascript to search and replace text. 

## How To

Replacing text can be done in a few simple steps with the help of Javascript. First, we need to define the text that we want to search for and replace. Let's say we want to replace all instances of the word "favorite" with "favourite" in a text.

```
// Define the text to search and replace
let text = "My favorite color is blue. My favorite food is pizza. My favorite movie is The Shawshank Redemption.";

// Use the replace() method to replace "favorite" with "favourite"
let newText = text.replace(/favorite/g, "favourite");

// Output the new text
console.log(newText);

// Output: My favourite color is blue. My favourite food is pizza. My favourite movie is The Shawshank Redemption.
```

In the above code, we use the replace() method and a regular expression to search for all instances of "favorite" and replace it with "favourite". Adding the "g" flag after the regular expression ensures that all occurrences of the word will be replaced, not just the first one.

We can also use the replace() method to replace multiple words at once. Let's say we want to replace "color" with "colour" and "food" with "cuisine". We can do this by passing in an object as the second parameter of the replace() method.

```
// Define the text to search and replace
let text = "My favorite color is blue. My favorite food is pizza. My favorite movie is The Shawshank Redemption.";

// Use the replace() method to replace "color" with "colour" and "food" with "cuisine"
let newText = text.replace(/color|food/g, function(match) {
  if (match === "color") {
    return "colour";
  } else if (match === "food") {
    return "cuisine";
  }
});

// Output the new text
console.log(newText);

// Output: My favorite colour is blue. My favourite cuisine is pizza. My favourite movie is The Shawshank Redemption.
```

## Deep Dive

Regular Expressions (regex) play a crucial role in searching and replacing text in Javascript. They allow us to search for patterns in strings and perform replacements based on those patterns. In the examples above, we used the "g" flag to replace all occurrences of a word. We can also use other flags such as "i" for case-insensitive matching.

Another useful feature of regex is the ability to use metacharacters. These are special characters that represent a group of characters. For example, the "." metacharacter represents any single character. So if we wanted to replace all words that start with the letter "m", we can use the regex "/m./g" where "m" represents the letter "m" and "." represents any single character.

```
// Define the text to search and replace
let text = "My favorite color is blue. My favorite food is pizza. My favorite movie is The Shawshank Redemption.";

// Use the replace() method with regex to replace all words that start with "m"
let newText = text.replace(/m./g, "amazing");

// Output the new text
console.log(newText);

// Output: My amazing color is blue. My amazing food is pizza. My amazing movie is The Shawshank Redemption.
```

See Also

- [MDN Web Docs - String replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [W3Schools - JavaScript regular expressions](https://www.w3schools.com/js/js_regexp.asp)