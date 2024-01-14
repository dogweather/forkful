---
title:                "Java recipe: Deleting characters matching a pattern"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why
Ever found yourself in a situation where you needed to delete specific characters from a string or a set of strings? Maybe you're working on a data cleaning project or just trying to modify some user inputs. Either way, knowing how to delete characters matching a pattern can significantly help in manipulating strings in Java.

## How To
Let's dive into some code examples and see how we can delete characters matching a pattern in Java.

```Java
// Define the original string
String originalString = "Th!is is a @test $string to #delete characters";

// Use the replaceAll() method to delete characters matching a pattern
String modifiedString = originalString.replaceAll("[^a-zA-Z0-9 ]", "");
// Output: Thisisateststringtodeletecharacters
```

In the above code, we used the `replaceAll()` method to delete all characters except letters, numbers, and spaces from the original string. The `[^a-zA-Z0-9 ]` pattern specifies to match any character that is not an alphabet, number, or space and replace it with an empty string. This way, we are left with only the characters we want to keep in our string.

We can also use regular expressions to match specific patterns of characters and delete them from the original string. For example:

```Java
// Define the original string
String originalString = "This is a test string to delete repetitive characters";

// Use regular expressions to delete repetitive characters
String modifiedString = originalString.replaceAll("(.)\\1+", "$1");
// Output: This is a test string to delete repetitive characters
```

In this code, we used the regular expression `(.)\\1+` to match any character that is repeated one or more times and replace it with the first instance of that character. This effectively deletes any repetitive characters from the original string.

## Deep Dive
Now that we have seen some examples of deleting characters matching a pattern in Java, let's take a closer look at what is happening behind the scenes.

The `replaceAll()` method uses the `String.replace()` method internally to find and replace characters. The only difference is that `replaceAll()` allows us to use regular expressions as patterns, making it more flexible for complex string manipulations.

Regular expressions, also known as regex, are sequences of characters that define a search pattern. In Java, we use the `Pattern` and `Matcher` classes to create a regex pattern and apply it to strings.

For our first example, the pattern `[^a-zA-Z0-9 ]` means any character that is not an alphabet, number, or space. The `^` symbol at the beginning of the pattern means the negation of the characters within the brackets. We can also use the `+` symbol to match one or more occurrences of the previous character, and the `*` symbol to match zero or more occurrences.

For our second example, the pattern `(.)\\1+` means any character that is repeated one or more times. The parentheses `()` allow us to group the characters, and the `\\1` refers to the first group, which is the first instance of the character. Finally, the `+` symbol means to match one or more occurrences of the previous group.

By understanding how regular expressions work, we can create more complex patterns to match and delete specific characters from strings.

## See Also
- [Java String Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Regular Expressions Tutorial](https://www.javatpoint.com/java-regex)
- [Java Pattern Class Documentation](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)