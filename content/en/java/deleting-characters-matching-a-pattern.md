---
title:                "Java recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered a situation where you needed to delete certain characters from a string in your Java program? Whether you're dealing with user input or data processing, removing unwanted characters can be a common task. In this blog post, we will discuss why someone may need to delete characters matching a pattern and how to accomplish it in Java.

## How To

First, let's understand the basic syntax for deleting characters from a string in Java:

```
String originalString = "Hello, world!";
String modifiedString = originalString.replaceAll("l", "");
System.out.println(modifiedString);
```

In the above example, we are using the `replaceAll()` method to delete all occurrences of the letter "l" from the original string. The method takes two parameters - the pattern to be replaced and the replacement string. In this case, we are passing an empty string as the replacement, effectively removing all occurrences of the "l" character.

We can also use regular expressions to specify a pattern of characters to be deleted. For instance, if we wanted to remove all special characters from a string, we could do so as follows:

```
String originalString = "H@e#l$lo!";
String modifiedString = originalString.replaceAll("[^a-zA-Z0-9]", "");
System.out.println(modifiedString);
```

In this example, we are using the `[^]` notation to specify a pattern of characters to be deleted. In this case, we are removing all characters except for letters and numbers.

## Deep Dive

The `replaceAll()` method uses regular expressions to find and replace patterns in a string. This gives us a lot of flexibility in terms of the patterns we can specify. Here are a few more examples:

- Remove all vowels from a string: `originalString.replaceAll("[aeiou]", "")`
- Remove all non-ASCII characters from a string: `originalString.replaceAll("[^\\x00-\\x7F]", "")` 

Another important thing to note is that the `replaceAll()` method creates a new string instead of modifying the original one. This means that we need to assign the result to a new variable or reassign it to the original one if we want the changes to persist. 

## See Also

- [Java String replaceAll() method documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-) 
- [Java Regular Expressions tutorial](https://docs.oracle.com/javase/tutorial/essential/regex/)