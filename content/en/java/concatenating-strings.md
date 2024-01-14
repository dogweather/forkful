---
title:    "Java recipe: Concatenating strings"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings is an essential skill for Java programmers to master. It allows you to combine different pieces of text to create a new string, which is especially useful when working with user input or generating dynamic content.

## How To

To concatenate strings in Java, we can use the "+" operator or the `concat()` method. Let's look at some examples to better understand how this works.

```Java
// Using the "+" operator
String first = "Hello";
String second = "World";
String greeting = first + " " + second; // greeting = "Hello World"

// Using the `concat()` method
String fruit = "apple";
String color = "red";
String description = fruit.concat("s are ").concat(color); // description = "apples are red"
```

As we can see, both methods produce the same result - joining two or more strings together to create a new one.

But what happens if we want to concatenate more than two strings? In that case, we can use the `StringBuilder` class, which allows us to efficiently concatenate multiple strings.

```Java
StringBuilder sentence = new StringBuilder();
sentence.append("Java").append(" is").append(" a").append(" powerful").append(" language.");
System.out.println(sentence); // "Java is a powerful language."
```

We use the `append()` method to add each string to the `StringBuilder` object. This method also has an overload that accepts other data types, such as integers and booleans.

## Deep Dive

Behind the scenes, the "+" operator and the `concat()` method actually use the `StringBuilder` class. The difference is that the `StringBuilder` class can be reused to improve performance, while the other methods create a new `String` object every time.

It's also worth noting that strings are immutable in Java, meaning they cannot be changed after they are created. This is why the `StringBuilder` class is more efficient for concatenation since it allows us to modify the same object instead of creating a new one.

## See Also

If you want to learn more about string concatenation in Java, check out these helpful resources:

- [Oracle's Java String Documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [GeeksforGeeks Article on String Concatenation](https://www.geeksforgeeks.org/stringbuilder-class-in-java-with-examples/)
- [Concatenation vs StringBuilder Performance Comparison](https://www.baeldung.com/java-string-concatenation)