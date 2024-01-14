---
title:    "C# recipe: Deleting characters matching a pattern"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why

As a programmer, you may have encountered situations where you need to delete characters from a string based on a certain pattern. This could be for data cleaning, data manipulation, or any other task that involves dealing with strings. In these cases, having the knowledge of how to do this can save you time and effort.

## How To

To delete characters matching a pattern in C#, we can use the `Replace()` method. This method takes two parameters, the first being the pattern to match and the second being the replacement string. Let's take a look at an example:

```C#
string sentence = "I love coding!";
string newSentence = sentence.Replace("love", "enjoy");
Console.WriteLine(newSentence);
```

The output of this code would be: "I enjoy coding!"

In this example, the pattern we wanted to replace was "love" and the replacement string was "enjoy". The `Replace()` method then replaces all occurrences of "love" in the original sentence with "enjoy".

In some cases, we may want to delete characters from a string based on a more complex pattern. For this, we can use regular expressions. Regular expressions allow us to specify a pattern and perform various operations on strings that match that pattern.

Let's say we want to remove all vowels from a string. We can use the following regular expression:

```C#
string sentence = "Game of Thrones";
string newSentence = Regex.Replace(sentence, "[aeiou]", "");
Console.WriteLine(newSentence);
```

The output of this code would be: "Gm f Thrns".

In this example, the regular expression "[aeiou]" matches all vowels in the string and replaces them with an empty string, effectively deleting them.

## Deep Dive

While the `Replace()` method and regular expressions can handle most cases of deleting characters matching a pattern, it's important to understand how these methods actually work. The `Replace()` method uses a simple search and replace strategy, meaning it looks for an exact match of the specified pattern and replaces it with the given replacement string.

Regular expressions, on the other hand, offer a more powerful and flexible way to handle pattern matching and replacement. They use a pattern-matching engine to find all possible matches of the given pattern and allow for advanced operations such as capturing groups and quantifiers.

It's important to keep in mind that regular expressions can be complex and can sometimes have unexpected results. It's always a good idea to test and debug your regular expressions before using them in your code.

## See Also

- [C# String Manipulation Methods](https://www.dotnetperls.com/string-methods)
- [Regular Expressions in C#](https://www.c-sharpcorner.com/UploadFile/pranayamr/regular-expressions-in-C-Sharp/)
- [C# Regex Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)