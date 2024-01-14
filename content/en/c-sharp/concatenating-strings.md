---
title:    "C# recipe: Concatenating strings"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
String concatenation is a common task in many programming languages, and this is no different in C#. Concatenating strings allows you to combine multiple strings into one, making it easier to manipulate and display data in your program.

## How To
In C#, there are a few different ways to concatenate strings, but the most common method is by using the "+" operator. Here is an example:

```C#
string firstName = "John";
string lastName = "Smith";
string fullName = firstName + " " + lastName;
Console.WriteLine(fullName);
```

This code snippet will output "John Smith". As you can see, the "+" operator allows us to combine multiple strings into one. You can also use the "+=" operator to append a string to an existing string variable. Here's an example:

```C#
string fullName = "John Smith";
fullName += " Jr.";
Console.WriteLine(fullName);
```

This will output "John Smith Jr.". Similarly, you can use the Concat() method in the String class to concatenate multiple strings:

```C#
string firstName = "John";
string lastName = "Smith";
string fullName = String.Concat(firstName, " ", lastName);
Console.WriteLine(fullName);
```

This will also output "John Smith". Additionally, you can use the Format() method to concatenate multiple strings with placeholders, making it easier to format your output:

```C#
string firstName = "John";
string lastName = "Smith";
string fullName = String.Format("{0} {1}", firstName, lastName);
Console.WriteLine(fullName);
```

This will also output "John Smith". It's important to note that you can also use the "+" operator and the Concat() method to concatenate non-string values, such as integers or decimals. In this case, C# will automatically convert those values into strings before concatenating them.

## Deep Dive
Behind the scenes, string concatenation in C# is achieved by using the StringBuilder class. This class provides better performance for string manipulation tasks, as strings in C# are immutable (which means they cannot be changed once created). When you use the methods or operators mentioned above, the compiler will actually create a new StringBuilder object and use the Append() method to concatenate your strings.

If you are concatenating a large number of strings, using the StringBuilder class directly will provide better performance than using the methods mentioned above. Here's an example of using the StringBuilder class:

```C#
StringBuilder sb = new StringBuilder();
sb.Append("John");
sb.Append(" ");
sb.Append("Smith");

string fullName = sb.ToString();
Console.WriteLine(fullName);
```

This will also output "John Smith". The advantage of using the StringBuilder class is that it allows you to manipulate your strings without creating new string objects every time.

## See Also
- [String.Concat method documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat)
- [StringBuilder class documentation](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder)
- [String.Format method documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string.format)