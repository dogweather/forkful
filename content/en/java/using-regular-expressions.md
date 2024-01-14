---
title:    "Java recipe: Using regular expressions"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why 

Regular expressions, also known as regex, are a powerful tool for pattern matching in programming. They can be used in a variety of tasks such as searching, replacing, and validating text. Regular expressions allow for efficient and precise manipulation of strings, making them an essential skill for any programmer.

## How To

To use regular expressions in Java, we first need to import the `Pattern` and `Matcher` classes from the `java.util.regex` package. Let's say we have a string containing a phone number and we want to validate it to make sure it follows the correct format. We can do this using regular expressions as shown below:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

String phoneNumber = "(123)456-7890";
String regex = "\\(\\d{3}\\)\\d{3}-\\d{4}";

Pattern pattern = Pattern.compile(regex);
Matcher matcher = pattern.matcher(phoneNumber);

if(matcher.matches()){
    System.out.println("Valid phone number!");
} else {
    System.out.println("Invalid phone number.");
}
```

In this example, we first create a `Pattern` object with our regular expression as an argument. The `Matcher` object is then used to match our input string against the pattern. The `matches()` method returns a boolean value indicating whether the input string matches the pattern or not.

Another common use for regular expressions is extracting specific information from a string. Let's say we have a string containing a list of email addresses and we want to extract all the email domains from it. We can achieve this using the `find()` and `group()` methods as shown below:

```java
String emails = "john@gmail.com, jane@yahoo.com, bob@outlook.com";
String regex = "\\w+@(\\w+\\.\\w+)";

Pattern pattern = Pattern.compile(regex);
Matcher matcher = pattern.matcher(emails);

while(matcher.find()){
    String domain = matcher.group(1);
    System.out.println(domain);
}
// Output:
// gmail.com
// yahoo.com
// outlook.com
```

In this example, we use a capturing group `(\w+\.\w+)` to extract only the domain part of the email. The `find()` method is used to find the next match in the string, while the `group()` method returns the matched group.

Regular expressions also support a variety of metacharacters and quantifiers to make pattern matching more flexible. For a comprehensive reference on all the available metacharacters and their meanings, check out the [Java Regular Expressions Tutorial](https://docs.oracle.com/javase/tutorial/essential/regex/).

## Deep Dive 

When using regular expressions, it's important to be mindful of their performance. While they are a powerful tool, they can also be expensive to execute, especially on larger strings. This is because regular expressions involve backtracking, which means the engine may have to try different combinations of the pattern to find a match. To avoid this, it's recommended to use specific patterns that can be matched in a linear fashion without backtracking.

Another thing to keep in mind is the use of escape characters in regular expressions. In Java, backslashes are used to escape special characters in strings. However, backslashes are also used as metacharacters in regular expressions, so we need to escape them twice. For example, to match a literal backslash, we need to use `\\\\` in the regular expression.

## See Also

For more resources on regular expressions in Java, check out the following links:

- [Oracle Java Regular Expressions Tutorial](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Regular Expressions Cheat Sheet for Java](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/pdf/)
- [RegexPlanet](https://www.regexplanet.com/) - a handy tool for testing and visualizing regular expressions