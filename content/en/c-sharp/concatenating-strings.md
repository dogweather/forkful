---
title:                "C# recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to combine different strings into one cohesive sentence or phrase? Maybe you're creating a dynamic greeting for a user, or building a complex sentence from different variables. Whatever the case may be, concatenating strings is a useful and essential skill to have in your C# programming arsenal.

## How To
Concatenating strings in C# is a fairly simple process. Here's an example of how to do it:

```C#
string firstName = "John";
string lastName = "Smith";
string greeting = "Hello, " + firstName + " " + lastName + "!";
Console.WriteLine(greeting);
```

The output of this code would be: `Hello, John Smith!` 

As you can see, we used the `+` operator to combine the different strings together. This is known as string concatenation. You can also use the `+=` operator to add onto an existing string. 

```C#
string username = "jsmith";
string website = "www.example.com";
website += "/"+ username;
Console.WriteLine(website);
```

The output of this code would be: `www.example.com/jsmith`

You can also use the `string.Concat()` method to concatenate multiple strings together. Here's an example:

```C#
string firstWord = "Hello";
string secondWord = "World";
string thirdWord = "!";
string phrase = string.Concat(firstWord, " ", secondWord, thirdWord);
Console.WriteLine(phrase);
```

The output of this code would be: `Hello World!`

## Deep Dive
Behind the scenes, when we use the `+` operator to concatenate strings, C# is using the `String.Concat()` method. This method takes in an array of strings and combines them together. So when we use the `+` operator, we are essentially creating an array of strings and passing it into the `String.Concat()` method.

Another important concept to note is that strings are immutable, meaning they cannot be changed. So when we use the `+` operator or the `String.Concat()` method to concatenate strings, a new string is created in memory instead of modifying the existing one. This can have implications on the performance of your code, especially when dealing with large strings. One way to improve performance is by using the `StringBuilder` class, which allows for efficient string manipulation.

## See Also
- [String Concatenation in C#](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/addition-operator)
- [String.Concat() Method Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat)
- [StringBuilder Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder)