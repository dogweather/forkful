---
title:                "C# recipe: Concatenating strings"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings may seem like a simple task, but it is an important skill to have in your programming arsenal. By combining multiple strings together, you can create dynamic and customizable output for your programs. Whether you are building a simple console application or a complex web application, understanding how to concatenate strings will greatly enhance your coding abilities.

## How To

To concatenate strings in C#, there are a few different methods you can use. One way is by using the "+" operator, like this:

```C#
string greeting = "Hello";
string name = "John";
string message = greeting + " " + name;

Console.WriteLine(message);
```

In this example, we have declared three string variables and concatenated them together using the "+" operator. The output would be "Hello John".

Another method is by using the `Concat()` method from the `String` class. Here is an example:

```C#
string city = "New York";
string state = "New York";
string location = String.Concat(city, ", ", state);

Console.WriteLine(location);
```

This will produce the same output as the first example, "New York, New York".

You can also use the `Format()` method to concatenate strings with additional formatting:

```C#
string firstName = "Jane";
string lastName = "Doe";
string fullName = String.Format("{0} {1}", firstName, lastName);

Console.WriteLine(fullName);
```

The output would be "Jane Doe".

## Deep Dive

Now, let's take a closer look at how concatenating strings actually works in C#. When using the "+" operator, the compiler will optimize it into a `Concat()` method call. This method takes in two or more strings and returns a new string with the concatenated values. So, essentially, the first two methods we discussed are just different ways of using the `Concat()` method.

It is worth noting that the `Concat()` method will not modify the original strings, but instead will create a new string with the combined values. This is important to keep in mind when dealing with large strings or performance-sensitive applications.

Another important concept to be aware of is string immutability. In C#, strings are immutable, meaning they cannot be changed once they are created. This means that every time we use the "+" operator or the `Concat()` method, a new string object is created in memory. So, if you have multiple concatenations in your code, it could potentially lead to memory issues.

To mitigate this, you can use the `StringBuilder` class which is mutable and allows for more efficient string concatenation.

## See Also

To learn more about concatenating strings in C#, check out these resources:

- [Microsoft Docs - String.Concat Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat?view=net-5.0)
- [C# Program - Concatenate strings](https://www.csharpstar.com/c-program-to-concatenate-strings/)
- [C# Corner - The Difference between String Concatenation and String Formatting](https://www.c-sharpcorner.com/UploadFile/97197d/the-difference-between-string-concatenation-and-string-form/)