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

## What & Why?

Concatenating strings in C# is the process of combining two or more string values together to create a new string. This is a common operation in programming, as it allows us to create dynamic text strings that can change based on different inputs or conditions.

Programmers use concatenation to build strings for messages, display information to users, or generate data for output. It allows for flexibility and customization in the final string output, making our programs more powerful and user-friendly.

## How to:

To concatenate strings in C#, we can use the "+" operator or the "String.Concat" method. Let's see some examples:

```C#
string firstName = "John";
string lastName = "Smith";

// Using the "+" operator
string fullName = firstName + " " + lastName;
Console.WriteLine(fullName); // Output: John Smith

// Using the "String.Concat" method
string fullName2 = String.Concat(firstName, " ", lastName);
Console.WriteLine(fullName2); // Output: John Smith
```

We can also concatenate multiple strings at once by separating them with commas inside the "String.Concat" method:

```C#
string firstName = "John";
string middleName = "David";
string lastName = "Smith";

// Concatenating multiple strings
string fullName = String.Concat(firstName, " ", middleName, " ", lastName);
Console.WriteLine(fullName); // Output: John David Smith
```

## Deep Dive:

Historically, string concatenation was a more complex process in programming languages, requiring specific methods or functions to combine strings. However, with the introduction of the "+" operator in C#, string concatenation has become more intuitive and straightforward.

In addition to the "+" operator and the "String.Concat" method, C# also offers the "String.Format" method for string concatenation. This method allows us to insert placeholders within a string, which are then replaced by specific values at runtime:

```C#
string firstName = "John";
string lastName = "Smith";
int age = 30;

// Using String.Format for concatenation
string message = String.Format("Hello, my name is {0} {1} and I am {2} years old.", firstName, lastName, age);
Console.WriteLine(message); // Output: Hello, my name is John Smith and I am 30 years old.
```

There are also other alternatives to string concatenation, such as using string interpolation with the "$" symbol, or using the "StringBuilder" class for more complex concatenation scenarios.

## See Also:

To learn more about string concatenation in C#, check out these resources:

- Microsoft documentation on string concatenation: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/
- C# string concatenation tutorial: https://www.tutorialspoint.com/csharp/csharp_string_concatenation.htm