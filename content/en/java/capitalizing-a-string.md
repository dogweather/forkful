---
title:                "Java recipe: Capitalizing a string"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered a situation where you needed to capitalize a string in your Java program? It may seem like a simple task, but it can be easy to overlook the proper way to do it. In this blog post, we'll explore the reasons why capitalizing a string is important and how to do it correctly in Java.

## How To

Capitalizing a string in Java involves converting the first letter of each word in the string to uppercase. Here's an example of how to do it using the `toUpperCase()` method:

```Java
String str = "hello world";
str = str.toUpperCase();
System.out.println(str);
```

The output of this code would be "HELLO WORLD". It's important to note that this method will only capitalize the first letter of each word, so if you want to capitalize every letter in the string, you'll need to use a loop.

Another way to capitalize a string is by using the `capitalize()` method from the `StringUtils` class in the Apache Commons library. This method will capitalize the first letter of the string, but it also takes into account words that are already capitalized, such as acronyms.

```Java
String str = "hello WORLD";
str = StringUtils.capitalize(str);
System.out.println(str);
```

The output of this code would be "Hello WORLD", with the first letter of the first word capitalized and the rest of the string unchanged.

## Deep Dive

Now that we've explored how to capitalize a string in Java, let's take a deeper dive into why it's important to do so. For one, it helps with readability and consistency in your code. When displaying user input or other data in your program, having properly capitalized strings can make the information easier to understand.

Additionally, some programming languages are case sensitive, meaning that capitalization matters when comparing strings or variables. By following the standard convention of capitalizing strings, you can avoid potential errors or confusion in your code.

## See Also

- [Java String methods](https://www.w3schools.com/java/java_ref_string.asp)
- [Apache Commons StringUtils class](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)

So next time you need to capitalize a string in your Java program, remember to consider why it's important and use the appropriate method for your needs. Happy coding!