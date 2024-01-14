---
title:                "C# recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

When working with text data in programming, it is common to encounter unwanted characters that do not fit the desired pattern. This can disrupt the functionality of the code and make it difficult to manipulate the data. In these cases, it is helpful to know how to delete characters that match a specific pattern, saving time and improving the overall quality of the code.

## How To

In C#, there are various methods for deleting characters based on a specific pattern. One way to do this is by using the `Regex.Replace()` method. This method takes in the string to search, the regular expression pattern, and the replacement string. For example, let's say we have a string variable named `text` that contains the text "Hello, World!". If we want to delete all commas from this string, we can use the following code:

```C#
text = Regex.Replace(text, ",", "");
```

This will replace all instances of the comma character with an empty string, effectively deleting them from the original string. The resulting value of `text` will be "Hello World!". 

Another way to delete characters based on a pattern is by using the `StringBuilder` class. This class allows us to create a mutable string, which means we can modify it as needed. We can use its `Remove()` method to delete characters at a specific position based on the pattern. For example, let's say we have the same string "Hello, World!" and we want to delete the first comma. We can use the following code:

```C#
StringBuilder sb = new StringBuilder(text);
sb.Remove(sb.indexOf(","), 1);
text = sb.ToString();
```

The `indexOf()` method finds the first occurrence of the specified character, and then we use the `Remove()` method to delete that character from the `StringBuilder` object. Finally, we assign the modified string back to the original variable. The resulting value of `text` will be "Hello World!".

## Deep Dive

While the above methods are useful for deleting characters based on a specific pattern, there are certain cases where more complex patterns may require a custom solution. In these cases, we can use the `Regex.Replace()` method with a callback function. This function allows us to specify custom logic for what should replace the matched pattern. For example, let's say we want to delete all vowels from a string. We can use the following code:

```C#
text = Regex.Replace(text, "[aeiou]", match => "");
```

The `[aeiou]` is a regular expression pattern that matches any lowercase vowel. In the callback function, we simply return an empty string, effectively deleting the matched character. The resulting value of `text` will be "Hll, Wrld!".

## See Also

- [C# Regex.Replace method](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace?view=net-5.0)
- [C# StringBuilder class](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-5.0)
- [Regular Expressions in C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)