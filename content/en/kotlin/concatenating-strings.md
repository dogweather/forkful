---
title:                "Kotlin recipe: Concatenating strings"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why: The Importance of Concatenating Strings

String concatenation is a fundamental concept in programming that involves combining multiple strings into one. It may seem like a simple task, but understanding how to concatenate strings properly can greatly improve the efficiency and readability of your code. In this blog post, we will explore the reasons why concatenating strings is an important skill to have in your programming repertoire.

## How To: Combining Strings in Kotlin

String concatenation in Kotlin is straightforward and can be achieved using the "+" operator. This operator allows you to join two or more strings together, creating a new string that contains all the combined characters. Let's take a look at some examples:

```
Kotlin val greeting = "Hello" val name = "John" val message = greeting + name // Output: "HelloJohn"
```
In this example, we first declared two string variables: `greeting` and `name`. Then, using the "+" operator, we combined them into a new string variable `message`. This new string will contain the value "HelloJohn" which is the result of combining the two strings.

String concatenation can also be used in more complex scenarios, such as combining strings with other data types. For example:

```
Kotlin val age = 30 val message = "I am " + age + " years old." // Output: "I am 30 years old."
```
In this case, we are concatenating not just two strings, but also an integer data type. The end result is a string that contains the value "I am 30 years old.".

Another useful feature in Kotlin is the ability to use string templates for concatenation. String templates allow you to embed expressions or variables inside a string, eliminating the need for the "+" operator. Let's see an example:

```
Kotlin val firstName = "Jane" val lastName = "Smith" val message = "My name is $firstName $lastName" // Output: "My name is Jane Smith"
```
Here, the variables `firstName` and `lastName` are inserted into the string using the `$` sign. This method is not only more concise, but it also helps to ensure that the correct data types are being concatenated.

## Deep Dive: Using StringBuilder for Efficient Concatenation

While the "+" operator and string templates are convenient ways to concatenate strings, they can be inefficient when dealing with large amounts of data. This is because each time you use the "+" operator, a new string object is created, taking up more memory. To avoid this, Kotlin offers the `StringBuilder` class for concatenating strings efficiently.

The `StringBuilder` class allows you to append strings as you go, without creating multiple string objects. This can greatly improve the performance of your code and is especially useful when concatenating strings in a loop. Let's see an example:

```
Kotlin val numbers = listOf("one", "two", "three") val stringBuilder = StringBuilder() numbers.forEach { stringBuilder.append("$it ") } val result = stringBuilder.toString() // Output: "one two three"
```

In this example, we have a list of strings called `numbers` and a `StringBuilder` object. We use the `forEach` function to loop through each item in the `numbers` list and append it to the `StringBuilder` object. Finally, we convert the `StringBuilder` object into a string using the `toString()` function and store it in the `result` variable.

## See Also

- [Kotlin String Concatenation](https://www.jetbrains.com/help/kotlin/basic-types.html#strings)
- [Kotlin StringBuilder](https://www.geeksforgeeks.org/stringbuilder-in-kotlin/)
- [Kotlin String Templates](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)