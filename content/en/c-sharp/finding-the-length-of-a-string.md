---
title:                "C# recipe: Finding the length of a string"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

In the world of programming, there are times when you need to know the exact number of characters in a string. Whether you're working with user input, manipulating text, or performing other tasks, being able to find the length of a string is a basic yet crucial skill for any developer. In this blog post, we will explore how to do this in C# and why it is important.

## How To

Finding the length of a string in C# is quite simple, thanks to the built-in `Length` property of the `String` class. Let's take a look at an example:

```C#
string myString = "Hello World";
int length = myString.Length;
Console.WriteLine(length);
```

This code will output `11`, which is the number of characters in the string "Hello World". As you can see, all you need to do is call the `Length` property on the string object and assign it to an integer variable.

You can also use the `Length` property in other ways, such as checking if a string is empty:

```C#
string emptyString = "";
int length = emptyString.Length;

if (length == 0)
{
    Console.WriteLine("This string is empty.");
}
```

In this case, the output will be "This string is empty." because the length of the string is 0. This is just one example of how knowing the length of a string can be useful in your code.

## Deep Dive

You may be wondering, how does the `Length` property actually work? Well, in C#, strings are stored as arrays of characters. This means that each character in the string is assigned an index, starting at 0. The `Length` property simply returns the number of characters in the array, giving you the length of the string.

It's also worth noting that the `Length` property only counts the actual characters in the string, not including any spaces or other special characters.

## See Also

- [Microsoft documentation on String.Length](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-5.0)
- [C# strings tutorial](https://www.c-sharpcorner.com/learn/c-sharp-string-basics)

Finding the length of a string may seem like a small task, but it is an important skill to have in your coding arsenal. With the `Length` property in C#, you can easily count the number of characters in a string and use it to enhance your code. We hope this blog post has been helpful in understanding how to find the length of a string in C#. Happy coding!