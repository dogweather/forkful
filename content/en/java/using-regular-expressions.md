---
title:                "Using regular expressions"
html_title:           "Java recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions, also known as regex, are a powerful tool used by programmers to manipulate and search for specific patterns within text strings. They use a concise and flexible syntax to describe what to search for, making it easier and more efficient to perform complex string operations. Regular expressions are useful for tasks such as data validation, text parsing, and data extraction, making them an essential skill for any programmer.

## How to:
To use regular expressions in your Java code, you first need to import the `java.util.regex` package. Then you can use the `Pattern` and `Matcher` classes to describe and match patterns within a string. Let's see an example of how to search for a specific email address within a text:

```Java
import java.util.regex.*;

String text = "My email is example@website.com";
String regex = "\\w+@\\w+\\.com";

Pattern pattern = Pattern.compile(regex); //compiles the regex into a pattern
Matcher matcher = pattern.matcher(text); //creates a matcher for the given text
if (matcher.find()) { //checks if the regex is found within the text
  System.out.println("Email address found: " + matcher.group()); //prints the matched text
}
```

Output:
```
Email address found: example@website.com
```

This example uses the following regex pattern: `\w+@\w+\.com`. Let's break it down:
- `\w+` matches one or more alphanumeric characters (letters, numbers, underscores)
- `@` matches the "@" character literally
- `\w+` matches one or more alphanumeric characters again
- `\.` matches the "." character literally (since "." is a special character in regex, it needs to be escaped with a backslash)
- `com` matches the string "com" literally

This is just a simple example of a regex, but the possibilities are endless. With regular expressions, you can perform more complex searches and manipulations, such as replacing text, splitting strings, and more.

## Deep Dive
Regular expressions were invented by computer scientist Stephen Kleene in the 1950s as a way to describe patterns in formal languages. They were later popularized by Unix tools like `grep` and `sed` in the 1970s. Today, regular expressions are widely used in many programming languages, including Java.

While regular expressions are a powerful tool, they do have some alternatives. One alternative is using the `String` class's built-in methods, such as `contains()` and `replaceAll()`. These are easier to use but can become cumbersome for more complex patterns. Another alternative is using third-party libraries such as Apache Commons, which offer more advanced features for handling regular expressions.

When using regular expressions in Java, there are a few implementation details to keep in mind. First, the `Pattern` class is immutable, meaning that once a pattern is compiled, it cannot be changed. Also, regular expressions use a greedy matching algorithm, meaning that they will match the longest possible string that matches the pattern. To make it non-greedy, you can use the `?` operator after quantifiers, such as `*?` or `+?`.

## See Also
- [Oracle's regular expressions tutorial](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Java's Pattern class documentation](https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html)
- [Java's Matcher class documentation](https://docs.oracle.com/javase/10/docs/api/java/util/regex/Matcher.html)