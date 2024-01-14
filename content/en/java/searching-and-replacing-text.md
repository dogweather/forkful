---
title:                "Java recipe: Searching and replacing text"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Searching and replacing text is a common task in programming, especially when working with large amounts of data or text. This process allows developers to efficiently make changes or edits to multiple instances of a specific text or pattern. It can save time and ensure consistency in the codebase.

## How To

To demonstrate how to search and replace text in Java, we will be using the String class `replace()` method. This method replaces all occurrences of a given character or string with another specified character or string. Let's take a look at an example:

```
String originalText = "Hello World!";
String replacedText = originalText.replace("o", "0");

System.out.println(replacedText);
```

This code snippet will output `Hell0 W0rld!`, as all instances of "o" in the original text have been replaced with "0". We can also use this method to replace multiple characters at once, for example:

```
String originalText = "This is a test.";
String replacedText = originalText.replace("is", "was");

System.out.println(replacedText);
```

The output for this code will be `Thwas was a test.`, as both instances of "is" have been replaced with "was".

## Deep Dive

The `replace()` method is a simple and straightforward way to search and replace text in Java. This method also accepts regular expressions as parameters, allowing for more complex replacements. Regular expressions are patterns used to match character combinations in strings. For example, we can use a regular expression to replace all numbers in a string with "x":

```
String originalText = "12345";
String replacedText = originalText.replace("[0-9]", "x");

System.out.println(replacedText);
```

The output will be `xxxxx`, as all numbers in the original string have been replaced with "x".

It is also worth noting that the `replace()` method does not modify the original string, but rather returns a new string with the replacements. This means that we can store the result of the method in a new variable, as shown in the examples above.

For more information on regular expressions in Java, check out the [official documentation](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html).

## See Also

- [Java String class documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [String replace() method in Java](https://www.geeksforgeeks.org/string-replace-method-in-java/)