---
title:                "Java recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, also known as regex, are a powerful tool used in Java programming for pattern matching and manipulation of strings. They allow for efficient and complex string operations, making them a valuable skill to have in your programming toolkit.

## How To

To use regular expressions in Java, you must first import the `java.util.regex` package. Then, you can create a `Pattern` object by using the `compile()` method and passing in a string representing the pattern you want to match.

Let's look at an example. Say we have a string that contains phone numbers in the format of (XXX) XXX-XXXX. We want to extract all the phone numbers from the string and store them in an array. We can do this using regular expressions.

````Java
import java.util.regex.*; // import the regex package

String text = "These are some phone numbers: (123) 456-7890, (456) 789-0123, (789) 012-3456";
String pattern = "\\(([0-9]{3})\\) ([0-9]{3}-[0-9]{4})"; // regex pattern to match phone numbers in the specified format

Pattern p = Pattern.compile(pattern); // create a Pattern object using the compile() method
Matcher m = p.matcher(text); // create a Matcher object to perform matching operations on the given text

List<String> phoneNumbers = new ArrayList<String>(); // create an array to store the phone numbers

while (m.find()) { // loop through the text and find matches
    phoneNumbers.add(m.group()); // add the matched phone number to the array
}

System.out.println(phoneNumbers); // print the array of phone numbers
````

### Output
````Java
[(123) 456-7890, (456) 789-0123, (789) 012-3456]
````

## Deep Dive

Regular expressions in Java use a special syntax to represent patterns. Some of the common symbols used in regular expressions are:

- `.` - matches any single character
- `*` - matches zero or more of the preceding character
- `+` - matches one or more of the preceding character
- `?` - matches zero or one of the preceding character
- `[ ]` - matches any one of the characters within the brackets
- `^` - matches the beginning of the input string
- `$` - matches the end of the input string
- `{ }` - matches the specified number of occurrences of the preceding character

There are also special character classes that represent common patterns, such as `\d` for a digit character and `\w` for a word character.

It is important to note that regular expressions in Java are case-sensitive by default, but this behavior can be changed by using the `CASE_INSENSITIVE` flag.

Regular expressions can also be used for string replacement and splitting. The `replaceAll()` and `split()` methods from the `String` class can be used with regular expressions to perform these operations.

## See Also

- [Java Regular Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Regular Expressions - Wikipedia](https://en.wikipedia.org/wiki/Regular_expression)
- [Regexr - Regular Expression Tester](https://regexr.com/)