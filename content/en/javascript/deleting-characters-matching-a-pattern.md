---
title:                "Javascript recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Deleting characters that match a specific pattern can be a useful tool for data cleaning and manipulation in programming. It allows you to quickly and efficiently remove unwanted characters from strings, making your code more accurate and readable.

## How To

To delete characters matching a pattern in Javascript, you can use the `replace()` method. Here is an example of how to use it:

```Javascript
// Define the string with the unwanted characters
let string = "Hello, world!";

// Use regular expressions to define the pattern you want to remove
let pattern = /[^\w\s]/g;

// Use the replace() method to remove the matching characters and assign the result to a new variable
let newString = string.replace(pattern, "");

// Output the new string without the unwanted characters
console.log(newString); // Output: Hello world
```

In this example, we created a regular expression with the pattern `[^\w\s]` to match any non-alphanumeric and non-whitespace characters. We then used the `replace()` method to remove those characters from our string and assigned the result to a new variable `newString`. Finally, we printed the new string without the unwanted characters using `console.log()`.

You can also use this method to selectively replace characters based on a pattern. For example, if you want to remove all punctuation except for commas and periods, you can define the pattern as `/[^.,\s]/g`.

## Deep Dive

The `replace()` method in Javascript can also take in a callback function as the second argument. This allows for more complex and dynamic replacement logic based on the matched pattern. Here is an example:

```Javascript
let string = "The quick brown fox jumped over the lazy dog";

// Use a callback function in the replace() method to capitalize all letters that come after the word "the"
let newString = string.replace(/(the\s)(\w)/g, (match, group1, group2) => {
  return group1.toUpperCase() + group2.toUpperCase();
});

console.log(newString); // Output: THE QUICK BROWN FOX JUMPED OVER THE LAZY DOG
```

In this example, we used the regular expression `(the\s)(\w)` to match the word "the" followed by one letter. Then, in the callback function, we converted both the matching groups to uppercase and returned them. This allowed us to capitalize all letters that come after the word "the" in our string.

## See Also

- [Javascript Documentation on `replace()` method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Regular Expressions in Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)