---
title:    "Java recipe: Deleting characters matching a pattern"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Why

There may come a time in your Java programming journey where you need to quickly and efficiently delete characters that match a specific pattern in a string. This could be for data cleaning, text manipulation, or even security purposes. In this blog post, we will discuss how to do this in Java and provide some tips for a deeper understanding of the topic.

## How To

Let's start with the basics. To delete characters matching a pattern in Java, we will use the `replaceAll()` method from the `String` class. This method takes two parameters: the regex pattern and the replacement string. The regex pattern is used to match the characters we want to delete, and the replacement string is used to replace those characters with nothing (essentially deleting them).

Here's an example of using `replaceAll()` to delete all vowels from a string:

```Java
String str = "Hello World";
str = str.replaceAll("[aeiou]", ""); // deletes all lowercase vowels
System.out.println(str); // prints "Hll Wrld"
```

As you can see, the `replaceAll()` method takes a regex pattern as a string. In this case, we used a character class to match any lowercase vowel. We can also use literal strings as the pattern, like so:

```Java
String str = "1-800-CALL-NOW";
str = str.replaceAll("-", ""); // deletes all hyphens
System.out.println(str); // prints "1800CALLNOW"
```

If we want to delete characters that are not included in a specific pattern, we can use the `^` symbol at the beginning of the character class. For example, if we want to delete all non-numeric characters from a string, we can use the following code:

```Java
String str = "(555) 123-4567";
str = str.replaceAll("[^0-9]", ""); // deletes all non-numeric characters
System.out.println(str); // prints "5551234567"
```

## Deep Dive

While the `replaceAll()` method is great for simple string substitutions, it can be more complex when it comes to deleting Unicode characters or special characters. In these cases, we can use the `replace()` method from the `StringBuilder` class.

The `replace()` method takes two parameters: the starting index and the ending index for the characters we want to replace. We can also chain multiple calls to this method to delete multiple patterns within a string. Here's an example:

```Java
String str = "Hello, world!";
StringBuilder sb = new StringBuilder(str);
sb.replace(7, 13, ""); // deletes "world"
sb.replace(0, 6, ""); // deletes "Hello, "
str = sb.toString();
System.out.println(str); // prints "!"
```

In this example, we first delete the substring "world" by specifying the starting and ending indices. Then, we delete the remaining characters by specifying the starting and ending indices again. The final string is an empty string, indicating that we successfully deleted all matching patterns.

## See Also

- [Java String replaceAll() method](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replaceAll-java.lang.String-java.lang.String-)
- [Java StringBuilder replace() method](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html#replace-int-int-java.lang.String-)
- [Regex tutorial](https://www.regular-expressions.info/tutorial.html)