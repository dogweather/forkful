---
title:    "C# recipe: Capitalizing a string"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why Capitalize a String in C#?

Capitalizing a string is a common task in many programming languages, including C#. It involves changing the case of the letters in a string so that the first letter is capitalized, while all other letters are lowercase. This can be useful for presenting data in a more visually appealing way or for following proper naming conventions in your code.

## How To Capitalize a String in C#

To capitalize a string in C#, you can use the `ToUpper()` method from the `String` class. This method converts all characters in a string to uppercase, but to only capitalize the first letter, we can combine it with the `Substring()` method to get the first character and then use `ToUpper()` on just that character.

```C#
string myString = "hello world";
myString = myString.Substring(0, 1).ToUpper() + myString.Substring(1).ToLower();

Console.WriteLine(myString); // Outputs: Hello world
```

In this example, we first create a string with all lowercase letters. Then, we use `Substring()` to get the first character (at index 0) and change it to uppercase using `ToUpper()`. Next, we use `Substring()` again to get all other characters in the string starting from index 1, and use `ToLower()` to make them lowercase. Finally, we assign the new string back to `myString` and print it out to see the result.

## Deep Dive into Capitalizing a String in C#

In C#, strings are immutable, meaning they cannot be changed. This means that when we call the `ToUpper()` or `ToLower()` methods, they do not modify the existing string, but instead create a new string with the desired changes. That's why in the code example above, we had to assign the result of the method calls back to the original string.

It's also worth noting that these methods use the current culture setting to determine the case of the characters. So depending on the culture of your system, the output of the capitalized string may vary. To override this behavior and use a specific culture, you can pass in a `CultureInfo` object as a parameter in the `ToUpper()` or `ToLower()` methods.

```C#
string myString = "hello world";
myString = myString.Substring(0, 1).ToUpper(CultureInfo.InvariantCulture) + myString.Substring(1).ToLower();

Console.WriteLine(myString); // Outputs: HELLO WORLD (culturally-invariant)
```

## See Also

For more information about working with strings in C#, you may find the following resources helpful:

- [String.ToUpper Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [String.ToLower Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [String.Substring Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- [C# Strings Tutorial (w3schools)](https://www.w3schools.com/cs/cs_strings.asp)