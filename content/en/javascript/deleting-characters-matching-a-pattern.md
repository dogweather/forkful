---
title:                "Javascript recipe: Deleting characters matching a pattern"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Why 
Have you ever encountered a situation where you needed to delete characters that match a specific pattern? This task is commonly faced in data cleaning and manipulation, where you may need to remove certain characters that are irrelevant to your data analysis. In this blog post, we will explore how to do this using Javascript, a popular programming language for web development.

# How To
To delete characters matching a pattern in Javascript, we can use the string `replace()` method along with a regular expression. Regular expressions are patterns used to match character combinations in strings, making it perfect for our task. Let's take a look at an example:

```Javascript
let str = "This is a sample string with some [special] characters";
let pattern = /\[.*?\]/g;
let result = str.replace(pattern, "");
console.log(result); // Output: This is a sample string with some characters
```

In the above code, we have a string `str` that contains a set of characters enclosed within square brackets. Our goal is to remove these brackets along with the characters inside them. To achieve this, we first define a regular expression `pattern` that matches any character within the square brackets. We use the `g` flag to indicate a global search, i.e. the pattern should be applied to the entire string. Finally, we use the `replace()` method to replace the matched characters with an empty string, effectively deleting them. The result is stored in the `result` variable and we log it to the console.

We can also use the `replace()` method along with a string as the replacement value. Let's see an example of this:

```Javascript
let str = "This is a sample string with some <special> characters";
let pattern = /<.*?>/g;
let result = str.replace(pattern, "brackets");
console.log(result); // Output: This is a sample string with some brackets characters
```

In this example, we use the `replace()` method to replace the characters enclosed within angle brackets with the word "brackets".

# Deep Dive
Regular expressions in Javascript can be quite complex and have various modifiers that can alter their behavior. For example, adding the `i` flag to our `pattern` variable in the first example would make the search case-insensitive, i.e. it would match both "[special]" and "[Special]". Similarly, the `m` flag can be used for multiline searches.

There are also various metacharacters that have special meanings in regular expressions. For example, the `.` metacharacter matches any single character, while the `*` metacharacter matches zero or more occurrences of the preceding character. Understanding these concepts and modifiers can help you create powerful regular expressions for your data manipulation tasks.

# See Also
- [MDN Web Docs: Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Regexr: Learn, Build, & Test RegEx](http://regexr.com/)
- [1-line Regular Expressions in Javascript](https://1loc.dev/#regular-expressions)