---
title:    "Java recipe: Concatenating strings"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Why
Concatenating strings is a commonly used operation in Java programming. It allows us to combine multiple strings together to create a new string. This can be useful when creating dynamic messages, building URLs, or any other scenario where we need to combine text.

## How To
To concatenate strings in Java, we can use the `+` operator or the `concat()` method. Let's take a look at some examples using both methods:

```Java
String firstName = "John";
String lastName = "Smith";

// Using the + operator
String fullName = firstName + " " + lastName;
System.out.println(fullName); // Output: John Smith

// Using the concat() method
String newFullName = firstName.concat(" ").concat(lastName);
System.out.println(newFullName); // Output: John Smith
```

In the examples above, we declare two string variables `firstName` and `lastName` and assign them values. Then, using either the `+` operator or `concat()` method, we combine the two variables and assign the result to a new string variable `fullName` and `newFullName` respectively.

We can also use the `+=` operator to add more strings to an existing string variable. Let's see an example:

```Java
String message = "Hello";
message += " World";
System.out.println(message); // Output: Hello World
```

In this example, we first initialize the string variable `message` with the value "Hello". Then, we use the `+=` operator to add the string " World" to the end of the existing value of `message`.

## Deep Dive
Behind the scenes, when we use the `+` operator or `concat()` method, Java creates a new string that contains the combined values of the two strings. This is because strings in Java are immutable, meaning they cannot be changed after they are created. So when we concatenate strings, we are actually creating a new string object rather than modifying the original ones.

It is worth noting that while using the `+` operator is convenient and easy to read, it can be less efficient compared to using the `StringBuilder` class. This is because the `StringBuilder` class is designed specifically for concatenating strings and can perform better when working with large amounts of text. Here is an example of using `StringBuilder`:

```Java
StringBuilder fullNameBuilder = new StringBuilder();
fullNameBuilder.append(firstName).append(" ").append(lastName);
String fullName = fullNameBuilder.toString();
System.out.println(fullName); // Output: John Smith
```

Behind the scenes, `StringBuilder` creates a mutable string buffer, allowing us to modify the string without creating a new object each time we add a new string.

## See Also
- [Java String Concatenation](https://www.geeksforgeeks.org/java-string-concatenation/)
- [StringBuilder vs StringBuffer in Java](https://www.baeldung.com/java-string-builder-vs-string-buffer)