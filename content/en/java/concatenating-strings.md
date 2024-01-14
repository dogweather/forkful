---
title:                "Java recipe: Concatenating strings"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings in Java is a fundamental skill that every programmer should have in their arsenal. Simply put, combining multiple strings into one allows for the creation of more complex and dynamic outputs. Whether you are a beginner or seasoned developer, understanding how to concatenate strings can greatly improve your coding abilities.

## How To

The syntax for concatenating strings in Java is relatively simple. First, you need to declare two or more strings that you want to combine. Let's use the example of combining a first and last name:

```
// First, we declare our two strings
String firstName = "John";
String lastName = "Smith";

// Next, we use the "+" operator to concatenate the two strings
String fullName = firstName + lastName;

// Lastly, we can print out the result
System.out.println(fullName); // Output: JohnSmith
```

As you can see, the "+" operator is used to join the two strings together. This can also be done with more than two strings, simply by adding them all together with the "+" operator.

```
// Combining three strings
String hello = "Hello, ";
String firstName = "John";
String lastName = "Smith";
String greeting = hello + firstName + lastName;

// Output: Hello, JohnSmith
```

In addition to using the "+" operator, there is another method for concatenating strings called the `concat()` method. This method allows you to join two strings together, just like the "+" operator.

```
// Using the concat() method
String hello = "Hello, ";
String world = "world!";
String greeting = hello.concat(world);

// Output: Hello, world!
```

It's important to note that the `concat()` method works a little differently when it comes to combining more than two strings. Take a look at the following example:

```
String hello = "Hello, ";
String firstName = "John";
String lastName = "Smith";

String greeting = hello + firstName.concat(lastName);

// Output: Hello, JohnSmith
```

In this case, the `concat()` method is only used on the "firstName" string, which is then combined with the "hello" string using the "+" operator.

## Deep Dive

Behind the scenes, concatenating strings in Java actually creates a new string object. Whenever you use the "+" operator or the `concat()` method, a new string is created with the concatenated result. This is because strings in Java are immutable, meaning they cannot be changed after they're created.

It's also worth mentioning that there is a more efficient way to concatenate large amounts of strings - using the `StringBuilder` class. This class allows for the creation of mutable strings, meaning they can be modified after they're created. This results in better performance when dealing with a large number of string concatenations.

## See Also

- [Java String documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [StringBuilder class documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/StringBuilder.html)
- [String concatenation in Java](https://www.geeksforgeeks.org/string-concatenation-in-java/)