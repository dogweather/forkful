---
title:    "C# recipe: Finding the length of a string"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to know the length of a string in your C# programming? Whether it's for validation, manipulation, or just curiosity, finding the length of a string is a common task that every developer should know how to do.

## How To
Finding the length of a string in C# is a simple and straightforward task. First, we need to declare a variable of type `string` and assign it a value.

```C#
string myString = "Hello World!";
```

Next, we can use the `Length` property of the string class to get the length of our string.

```C#
int length = myString.Length;
```

We can then use this value however we need in our code. For example, we can output it to the console to verify the length.

```C#
Console.WriteLine("The length of the string is: " + length);
```

The output will be:

```
The length of the string is: 12
```

Now, let's try it with an empty string.

```C#
string emptyString = "";
int length = emptyString.Length;
Console.WriteLine("The length of the string is: " + length);
```

The output will be:

```
The length of the string is: 0
```

As we can see, the `Length` property also works for empty strings, returning a value of 0.

## Deep Dive
The `Length` property of the string class is a built-in function that returns the number of characters in the string. It is a read-only property, meaning that we cannot assign a different value to it.

Additionally, the `Length` property only counts the number of characters in a string. It does not take into account any special characters or white spaces. For example, if we have a string with a whitespace at the end, the `Length` property will still only count the number of characters in the string.

There are also other useful string methods that we can use in conjunction with the `Length` property. For instance, we can use the `Substring()` method to extract a part of the string based on the length value. We can also use the `IndexOf()` method to find the position of a specific character within the string, which can be useful if we need to manipulate the string in different ways.

Overall, understanding how to find the length of a string is fundamental to C# programming and can be a useful skill in solving various coding problems.

## See Also
- [String.Length Property in C#](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=netframework-4.8)
- [String.Substring Method in C#](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=netframework-4.8)
- [String.IndexOf Method in C#](https://docs.microsoft.com/en-us/dotnet/api/system.string.indexof?view=netframework-4.8)