---
title:    "C# recipe: Finding the length of a string"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to know the length of a string in your C# program? Whether you're building a simple text processing application or a complex data manipulation tool, being able to determine the length of a string can be a valuable skill to have. In this blog post, we will explore why it is important to know the length of a string and how to go about finding it in your C# code.

## How To

To find the length of a string in C#, we will be using the built-in `Length` property of the `string` class. Let's take a look at an example of how this can be implemented in your code:

```C#
string message = "Hello World!";
int length = message.Length;
Console.WriteLine("The length of the string is: " + length);
```

In this example, we have a string variable named `message` with the value "Hello World!". We then use the `Length` property to get the length of the string and assign it to an `int` variable named `length`. Finally, we use `Console.WriteLine` to display the length to the console. 

The output of this code would be: `The length of the string is: 12`.

You can also use the `Length` property directly in operations, such as checking if a string has a certain length:

```C#
string message = "Hello World!";
if (message.Length > 10)
{
    Console.WriteLine("The string is longer than 10 characters.");
}
```

In this example, we use the `Length` property to check if the string has more than 10 characters. If it does, we print a message to the console. 

## Deep Dive

Internally, the `Length` property uses the `Length` property of the underlying `char` array of the string. This means that `Length` returns the number of `char`s in the string, not the number of bytes. This is important to keep in mind when working with Unicode characters, as they often require more than one byte to encode.

It is also worth noting that the `Length` property is a read-only property, so you cannot set it manually. It is automatically calculated based on the contents of the string.

Additionally, the `Length` property does not include the terminating null character of a string, so the actual length of the string may be one character longer than the `Length` value.

## See Also

- [Microsoft Docs - String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-5.0)
- [C# String Length](https://www.w3schools.com/cs/cs_string_length.asp)
- [C# String Class](https://www.c-sharpcorner.com/programming/introduction-to-string-class-in-C-Sharp)

Thank you for reading this blog post on finding the length of a string in C#! Remember to practice this skill in your own code and check out the additional resources for more in-depth information. Happy coding!