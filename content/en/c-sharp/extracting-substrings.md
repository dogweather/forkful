---
title:    "C# recipe: Extracting substrings"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why
Substring extraction is a common task in programming that can be used to manipulate and extract specific parts of a larger string. It can be especially useful when dealing with large amounts of data, such as in data processing applications or text parsing.

## How To
To extract a substring in C#, we can use the `Substring()` method. This method takes in two arguments: the starting index of the substring and the length of the desired substring. For example:

```C#
string myString = "Hello, world!";
string mySubstring = myString.Substring(0, 5);
Console.WriteLine(mySubstring);
```

The code above will output `Hello`, since it starts at index 0 and takes a substring with a length of 5 characters. We can also use this method to extract multiple substrings from a single string:

```C#
string myString = "This is a sentence.";
string myFirstSubstring = myString.Substring(0, 4);
string mySecondSubstring = myString.Substring(5, 2);
string myThirdSubstring = myString.Substring(8, 1);

Console.WriteLine(myFirstSubstring); // Outputs "This"
Console.WriteLine(mySecondSubstring); // Outputs "is"
Console.WriteLine(myThirdSubstring); // Outputs "a"
```

We can even use the `Substring()` method to extract a substring starting from the end of the string by using negative numbers. For example:

```C#
string myString = "abcde";
string mySubstring = myString.Substring(-3, 3);

Console.WriteLine(mySubstring); // Outputs "cde"
```

## Deep Dive
Under the hood, the `Substring()` method uses the `String.Substring()` method from the .NET Framework class `System.String`. This method takes in a start index and a length, and creates a new string with the desired substring. It does this by creating a new character array with the desired length and copying the characters from the original string starting at the given index. Understanding how this method works allows us to use it more effectively and efficiently in our code.

It's worth noting that the `Substring()` method does not modify the original string, instead it creates a new string with the extracted substring. This means that the original string remains unchanged, making this method a safe and convenient way to extract substrings without altering the original data.

## See Also
- Microsoft documentation for `Substring()` method: https://docs.microsoft.com/en-us/dotnet/api/system.string.substring
- C# String class: https://docs.microsoft.com/en-us/dotnet/api/system.string
- Codecademy tutorial on string manipulation: https://www.codecademy.com/learn/learn-c-sharp/modules/learn-csharp-manipulating-strings