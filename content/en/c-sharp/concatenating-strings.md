---
title:    "C# recipe: Concatenating strings"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why

When working with strings in C#, there may be situations where we need to combine multiple strings into one. This is where concatenation comes in handy. By joining strings, we can create more complex and dynamic text outputs in our code.

## How To

Concatenating strings in C# is a simple process. Let's take a look at an example:

```
string firstName = "John";
string lastName = "Doe";
string fullName = firstName + " " + lastName;

Console.WriteLine(fullName);
```

In this code, we have three strings - `firstName`, `lastName`, and `fullName`. We use the `+` operator to join the `firstName` and `lastName` strings with a space in between. Then, we assign the result to the `fullName` variable.

The output of this code would be "John Doe", as expected.

But what if we want to add more than just two strings? We can continue using the `+` operator to add as many strings as we need.

```
string firstLine = "Hello,";
string secondLine = "my name is";
string thirdLine = "John.";

string output = firstLine + " " + secondLine + " " + thirdLine;

Console.WriteLine(output);
```

The output of this code would be "Hello, my name is John.", with all three strings joined together.

We can also include variables and even numbers in our concatenation.

```
string message = "I have " + 3 + " cats.";
```

This would result in the string "I have 3 cats." being assigned to the `message` variable.

## Deep Dive

In C#, strings are immutable, which means they cannot be changed once they are created. So when we use the `+` operator to concatenate strings, what really happens is that a new string is created with the combined value of the original strings.

This can sometimes lead to performance issues when concatenating a large number of strings, as it creates multiple new objects in memory. In such cases, it is more efficient to use the `StringBuilder` class, which allows us to manipulate strings without creating new objects each time.

Another way to concatenate strings in C# is by using the `string.Format()` method or the `$` symbol (string interpolation). These methods allow us to insert variables directly into a string without having to use the `+` operator.

```
string fruit = "apple";
int quantity = 3;

// using string.Format()
string message = string.Format("I have {0} {1}s.", quantity, fruit);

// using string interpolation
string message = $"I have {quantity} {fruit}s.";
```

Both of the above methods would result in the string "I have 3 apples." being assigned to the `message` variable.

## See Also

- [Microsoft Docs on string concatenation](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/concatenate-multiple-strings)
- [C# StringBuilder Class](https://www.c-sharpcorner.com/UploadFile/mahesh/stringbuilder-class-in-C-Sharp/)
- [String Formatting in C#](https://www.c-sharpcorner.com/blogs/string-formatting-in-C-Sharp1)