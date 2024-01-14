---
title:    "Java recipe: Deleting characters matching a pattern"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Deleting characters matching a pattern is a common task in programming, especially when working with strings. This technique allows us to remove unwanted characters from a string, making our programs cleaner and more efficient.

## How To

To delete characters matching a pattern in Java, we can use the `replaceAll()` method from the `String` class. This method takes in two parameters: a regex pattern and a replacement string.

```Java
String text = "Hello, world! This is a test string.";
// Remove all punctuation from the string
String result = text.replaceAll("[^a-zA-Z0-9 ]", "");
System.out.println(result);
// Output: Hello world This is a test string
```

In this example, we used a regex pattern `[^a-zA-Z0-9 ]` to match all non-alphanumeric characters and spaces. The `^` symbol negates the character class, making it match everything except the characters inside the brackets. Then, we replaced those characters with an empty string, effectively deleting them from the original string.

We can also use the same method to delete specific characters from a string. Let's say we want to remove all the vowels from a string.

```Java
String name = "John Doe";
// Remove vowels from the string
String result = name.replaceAll("[aeiouAEIOU]", "");
System.out.println(result);
// Output: Jhn D
```

As you can see, the `replaceAll()` method is a powerful tool for deleting characters matching a pattern in a string.

## Deep Dive

Under the hood, the `replaceAll()` method uses the `Pattern` and `Matcher` classes from the `java.util.regex` package. These classes allow us to create and manipulate regular expressions in Java.

The `Pattern` class represents a compiled regular expression, while the `Matcher` class is responsible for matching the pattern against a given input string. The `Matcher` class also has methods like `find()` and `replaceFirst()` that we can use instead of `replaceAll()` if we only want to replace the first match in a string.

Regular expressions can be complex, with a wide range of symbols and special characters that have specific meanings. It's essential to have a good understanding of regular expressions before using them in your code.

## See Also

- [Java String documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
- [Java Regular Expressions tutorial](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Regex Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)