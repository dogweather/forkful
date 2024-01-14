---
title:                "C# recipe: Capitalizing a string"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why Capitalize a String
Capitalizing a string may seem like a small and insignificant task, but it is an important aspect of string manipulation in programming. By capitalizing a string, we are able to make the first letter of each word uppercase, which is necessary for proper grammar and readability. It is also often used in data validation and creating user-friendly interfaces.

## How To Capitalize a String in C#
To capitalize a string in C#, we can use the `ToUpper()` method. This method returns a new string with all the characters converted to uppercase. Here is an example of how we can capitalize a string in C#:

```C#
// Define a string
string message = "hello world";

// Capitalize the string
string capitalizedMessage = message.ToUpper();

// Output: HELLO WORLD
Console.WriteLine(capitalizedMessage);
```

Another approach is to use the `CultureInfo` class, which allows us to specify a culture and its language-specific capitalization rules. Here is an example using the `TextInfo.ToTitleCase` method:

```C#
// Define a string
string message = "hello world";

// Use the CultureInfo class to get language-specific capitalization rules
CultureInfo cultureInfo = CultureInfo.CurrentCulture;

// Capitalize the string
string capitalizedMessage = cultureInfo.TextInfo.ToTitleCase(message);

// Output: Hello World
Console.WriteLine(capitalizedMessage);
```

## Deep Dive into Capitalizing a String
While the `ToUpper()` and `ToTitleCase()` methods are simple and efficient ways to capitalize a string, there are other techniques that can be used as well. For instance, we can use a `StringBuilder` to change the first character of each word to uppercase. Additionally, we can create our own custom methods to handle special cases, such as acronyms or abbreviations.

Furthermore, when dealing with large amounts of text, it is important to consider efficiency when capitalizing a string. In such cases, using `String.ToUpper()` or `CultureInfo.TextInfo.ToTitleCase()` may not be the best choice, as they create a copy of the original string. Instead, we can use `TextInfo.ChangeCase()` method for better performance.

## See Also
Here are some additional resources on capitalizing a string in C#:
- [Microsoft documentation on string manipulation](https://docs.microsoft.com/en-us/dotnet/standard/base-types/string-manipulation)
- [CodeProject article on casing a string](https://www.codeproject.com/Articles/2129/Case-a-string-with-first-letter-of-each-word-in-cap)
- [Tutorial on creating custom string manipulation methods](https://www.tutlane.com/tutorial/csharp/csharp-string-functions-with-examples)

By utilizing these methods and techniques, we can effectively capitalize strings in C# and improve the readability and quality of our code. So the next time you come across a string that needs to be capitalized, you'll know exactly what to do. Happy coding!