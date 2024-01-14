---
title:    "C# recipe: Converting a string to lower case"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why

Converting a string to lower case is a common task in programming that allows for more efficient string processing. By converting all uppercase letters to lowercase, you can easily compare strings without having to worry about case sensitivity. This can be especially useful when working with user inputs or when dealing with data from external sources.

## How To

```C#
// Creating a string variable
string myString = "Hello WORLD!";

// Converting the string to lowercase
string lowerCaseString = myString.ToLower();

// Outputting the result
Console.WriteLine(lowerCaseString);

// Output: hello world!
```

Here, we have created a string variable containing the phrase "Hello WORLD!" and then used the `ToLower()` method to convert it to lowercase. The lowercase string is then stored in a new variable and outputted using `Console.WriteLine()`. The output of this code block will be "hello world!".

## Deep Dive

Converting a string to lowercase is handled by the `ToLower()` method, which is part of the `String` class in C#. This method uses the current system culture to determine the appropriate case mapping for the specified string. This means that the results may vary depending on the language and culture settings of the system.

It is also important to note that the `ToLower()` methoddoes not modify the original string, but rather returns a new string with the converted case. This is because strings in C# are immutable, meaning they cannot be changed after they have been created. So, when converting a string to lowercase, make sure to assign the result to a new variable or overwrite the original one.

## See Also

- [MSDN - String.ToLower Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [C# String Methods](https://www.tutorialsteacher.com/csharp/csharp-string-methods)
- [String Comparison in C#](https://www.c-sharpcorner.com/article/string-comparison-in-C-Sharp/)