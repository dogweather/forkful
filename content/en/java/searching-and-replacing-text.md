---
title:    "Java recipe: Searching and replacing text"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Have you ever come across a situation where you needed to find and replace a specific text in a large block of code or document? This can be a tedious and time-consuming task, especially if the text appears multiple times. But fear not, because Java has a built-in method for searching and replacing text that can save you time and effort. In this blog post, we will explore how to effectively search and replace text in Java.

## How To

To search and replace text in Java, we will be using the `replaceAll()` method from the `String` class. This method takes in two parameters - the text to be replaced and the new text to replace it with. Let's take a look at an example:

```Java
String message = "Hello world";
String newMessage = message.replaceAll("world", "Java");
System.out.println(newMessage);
```

The above code will replace the word "world" with "Java" in the `message` string and print out the new string - "Hello Java". Notice that the `replaceAll()` method returns a new string and does not modify the original string.

We can also use `replaceAll()` to replace multiple occurrences of a specific text. For example:

```Java
String sentence = "Java is awesome, Java is powerful";
String newSentence = sentence.replaceAll("Java", "Python");
System.out.println(newSentence);
```

The output will be "Python is awesome, Python is powerful". In this example, both occurrences of "Java" were replaced with "Python" in the `sentence` string.

We can also use regular expressions in the `replaceAll()` method to perform more complex replacements. For example, let's say we want to remove all the numbers from a string:

```Java
String sentence = "I have 10 apples, 5 bananas, and 3 oranges";
String newSentence = sentence.replaceAll("\\d+", "");
System.out.println(newSentence);
```

The output will be "I have apples, bananas, and oranges". In this example, we used the regular expression `\\d+` which matches any number of digits in a string. The `replaceAll()` method replaces all matches of the regular expression with an empty string, effectively removing the numbers from the `sentence` string.

## Deep Dive

Behind the scenes, the `replaceAll()` method uses the `Pattern` and `Matcher` classes from the `java.util.regex` package. These classes allow us to use regular expressions for more complex pattern matching and replacing. For those who are new to regular expressions, it may take some time to fully understand and utilize these classes. However, mastering them can greatly improve your text search and replace capabilities in Java.

It is also worth noting that the `replaceAll()` method is case-sensitive. If we want to ignore case while searching and replacing text, we can use the `replace()` method instead. This method also takes in two parameters - the text to be replaced and the new text to replace it with. The only difference is that it is not using regular expressions and is case-insensitive.

## See Also

For more information on regular expressions and the `replaceAll()` method, check out the following resources:

- [Oracle Java Documentation - String Class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
- [Oracle Java Documentation - Pattern Class](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Oracle Java Documentation - Matcher Class](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Matcher.html)
- [GeeksforGeeks Tutorial on Java Regex](https://www.geeksforgeeks.org/java-regex-tutorial/)

Now you have a better understanding of how to search and replace text in Java using the `replaceAll()` method. Happy coding!