---
title:                "Concatenating strings"
html_title:           "C# recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why 

Concatenating strings is an essential skill for any C# programmer. It allows you to dynamically create new strings by combining existing ones, making your code more flexible and efficient. Whether you're creating a program or working on a web application, knowing how to concatenate strings will greatly improve your coding experience. 

## How To

Concatenating strings in C# is a simple process. First, you need to declare two or more strings that you want to combine. Then, you can use the "+" operator to concatenate them together.

Let's see how this works with a simple example:

```C#
string firstName = "John";
string lastName = "Doe";
string fullName = firstName + " " + lastName;
Console.WriteLine(fullName);
```

In this example, we declare two strings, "firstName" and "lastName". Then, we use the "+" operator to concatenate them together and store the result in the "fullName" variable. Finally, we print out the variable to see the concatenated result: "John Doe".

You can also use the "string.Concat()" method to concatenate multiple strings at once. Here's an example:

```C#
string message = string.Concat("Hello", " ", "world!");
Console.WriteLine(message);
```

This will produce the same result as our previous example, "Hello world!".

## Deep Dive 

When it comes to concatenating strings, it's important to keep in mind that the "string" data type in C# is immutable. This means that once a string is created, it cannot be changed. So, when we use the "+" operator or the "string.Concat()" method to concatenate strings, we are actually creating a new string object in memory.

To avoid creating unnecessary objects and saving memory, it's recommended to use the "StringBuilder" class when working with a large number of string concatenations. The "StringBuilder" class allows you to modify strings without creating new objects, making it more efficient.

Another thing to note is that when using the "+" operator, strings are concatenated from left to right. So, if the first string is large, it can cause performance issues. To avoid this, it's better to use "string.Format()" or "string.Concat()" methods, which allow you to specify the order of the strings.

## See Also

If you want to learn more about string concatenation in C#, here are some helpful resources:

- [Microsoft Docs - String Concatenation](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/concatenate-multiple-strings)
- [C# Corner - Working with Strings in C#](https://www.c-sharpcorner.com/article/working-with-strings-in-C-Sharp/)
- [C# String Concatenation Tips and Tricks](https://www.codeguru.com/csharp-dotnet/csharp/strings-stringbuilder/string-concatenation-tips-and-tricks/)