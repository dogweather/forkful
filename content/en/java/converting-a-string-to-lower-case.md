---
title:                "Converting a string to lower case"
html_title:           "Java recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? 
Converting a string to lower case simply means changing all the characters in the string to their lower case equivalents. This is commonly done by programmers to improve string comparisons or to ignore capitalization in user input.

## How to:
To convert a string to lower case in Java, we can use the `toLowerCase()` method from the `String` class. Here's an example:
```Java
String name = "JOHN DOE";
String lowercaseName = name.toLowerCase();
System.out.println(lowercaseName); // Output: john doe
```
We can also convert individual characters to lower case using the `toLowerCase()` method from the `Character` class. Here's an example:
```Java
char letter = 'A';
char lowercaseLetter = Character.toLowerCase(letter);
System.out.println(lowercaseLetter); // Output: a
```

## Deep Dive:
Historically, case-insensitive string comparisons were done by converting the strings to upper case. However, this approach is not suitable for all languages, especially those with complex upper and lower case rules. Hence, the `toLowerCase()` method was introduced in Java version 1.0.

An alternative to using this method is to compare strings using the `equalsIgnoreCase()` method, which ignores case without changing the original string.

The `toLowerCase()` method uses the default locale's rules for converting characters to lower case. This can cause discrepancies when used in different locales. To avoid this, we can use the `toLowerCase(Locale)` method, which allows us to specify the locale to be used for conversion.

## See Also:
- [Java String Class](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [Java Character Class](https://docs.oracle.com/javase/7/docs/api/java/lang/Character.html)
- [Introduction to Locale in Java](https://docs.oracle.com/javase/tutorial/i18n/locale/index.html)