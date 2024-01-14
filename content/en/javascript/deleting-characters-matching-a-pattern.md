---
title:    "Javascript recipe: Deleting characters matching a pattern"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# Why Delete Characters Matching a Pattern in JavaScript

Deleting characters that match a specific pattern is a common task in JavaScript programming. Whether you are removing unwanted characters from user input or cleaning up a string before passing it to another function, knowing how to delete characters can save you time and improve the efficiency of your code.

# How To Delete Characters Matching a Pattern in JavaScript

The most common way to delete characters matching a pattern in JavaScript is by using the `replace()` function. This function takes in two parameters: the pattern to match and the replacement for the matched characters. For example, let's say we have a string that contains both letters and numbers, and we only want to keep the letters. We can use the following code:

```Javascript
let str = "H3ll0 W0rld"
let cleanedStr = str.replace(/[0-9]/g, "")
console.log(cleanedStr) // Output: HllWrld
```

In the above code, we use the `replace()` function with a regular expression as the first parameter. The regular expression `/[0-9]/g` matches all numbers in the string. The letter "g" at the end of the regular expression stands for global, which means it will match all instances of the pattern in the string. The second parameter is an empty string, indicating that we want to replace the matched characters with nothing. Hence, only the letters are left, and the numbers are deleted.

We can also use `replace()` to delete specific characters by using the `|` symbol to separate multiple patterns. For example, if we want to delete both numbers and symbols from a string, we can use the following code:

```Javascript
let str = "Hello@123!"
let cleanedStr = str.replace(/[0-9]|[@!#$%^&*]/g, "")
console.log(cleanedStr) // Output: Hello
```

In this example, we use the `|` symbol to indicate that the regular expression matches either numbers or symbols. Therefore, both are replaced with an empty string, resulting in only letters remaining in the string.

# Deep Dive into Deleting Characters Matching a Pattern

Regular expressions are a powerful tool for pattern matching and are commonly used in JavaScript for string manipulation. The `replace()` function is just one of many functions that allow you to use regular expressions to delete characters matching a pattern.

There are also other functions like `test()`, `search()`, and `split()` that can be used in combination with regular expressions to achieve the desired result. It's essential to understand how regular expressions work and to practice using them in your code to become proficient in pattern matching and manipulation.

# See Also

- [MDN - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Codecademy - Regular Expressions Cheat Sheet](https://www.codecademy.com/learn/learn-regular-expressions/modules/learn-regex-ruby/cheatsheet)