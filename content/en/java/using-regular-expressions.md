---
title:                "Java recipe: Using regular expressions"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

If you've ever encountered the task of searching, manipulating, or validating text, then regular expressions are a tool you need to know. They provide a powerful and efficient way to handle string manipulation in programming.

## How To

To use regular expressions in Java, the first step is to import the `java.util.regex` package. This package contains the necessary classes and methods for working with regular expressions. 

We can then create a `Pattern` object by compiling a given regular expression using the `compile()` method. This allows us to reuse the same regular expression in different parts of our code. 

Next, we need to create a `Matcher` object by invoking the `matcher()` method on our pattern and passing in the string we want to apply the regular expression on. This matcher object allows us to perform operations such as find and replace on the given string.

Let's see an example of finding a specific word in a sentence using regular expressions:

```Java
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class RegexDemo {
  public static void main(String[] args) {
    // compile our regular expression
    Pattern pattern = Pattern.compile("apple");

    // create a matcher object to work on our string
    Matcher matcher = pattern.matcher("I love apples!");

    // find the first occurrence of "apple" in the string
    if(matcher.find()) {
      System.out.println("Found apple in the string!");
    }
  }
}
```

The output of this code would be:

```
Found apple in the string!
```

Regular expressions also allow us to search for patterns within a string and extract specific parts of the string using capturing groups. For example, we can use the `group()` method on our matcher object to retrieve the matched word from our previous example:

```Java
// find and retrieve the matched word
String matchedWord = matcher.group();
System.out.println("The matched word is: " + matchedWord);
```

The output of this code would be:

```
The matched word is: apple
```

## Deep Dive

One of the main advantages of using regular expressions is their flexibility and power in handling complex string manipulations. Let's take a look at some symbols and modifiers that can help us build more robust regular expressions.

- `.` matches any single character except a line terminator.
- `\w` matches any word character (letters, digits, underscore).
- `+` matches one or more occurrences of the preceding pattern.
- `[]` matches any one of the characters enclosed within the brackets.
- `?` matches zero or one occurrences of the preceding pattern.
- `*` matches zero or more occurrences of the preceding pattern.
- `|` matches the pattern on the left or the pattern on the right.

These are just a few examples of the many symbols and modifiers available for regular expressions. It's important to note that regular expressions can be very complex and take some time to fully understand and master. However, the more you practice and work with them, the more comfortable you will become with using them in your code. 

## See Also

- [Java Documentation on Regular Expressions](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)
- [Regular-Expressions.info Tutorial](https://www.regular-expressions.info/tutorial.html)
- [Regex101 Online Regex Tester](https://regex101.com/)