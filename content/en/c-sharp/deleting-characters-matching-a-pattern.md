---
title:                "Deleting characters matching a pattern"
html_title:           "C# recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why
Deleting characters matching a pattern can be useful in various scenarios such as data cleaning and processing, removing unwanted characters from a string, or extracting specific information. By understanding how to delete characters matching a pattern in C#, you can efficiently manipulate data and improve the overall functionality of your code.

## How To
```C#
string text = "Hello123$World!";
string cleanText = Regex.Replace(text, @"[0-9$]", ""); // using Regex class

Console.WriteLine(cleanText); // output: HelloWorld
```

To delete characters matching a pattern in C#, we can use the Regex.Replace() method from the Regex class. This method accepts two parameters: the input string and the pattern to match. In the above example, we use a regular expression pattern to match all numbers and the dollar symbol ($) in the input string. Then, we replace them with an empty string, effectively deleting them from the string. 

Another way to achieve the same result is by using the StringBuilder class:

```C#
StringBuilder sb = new StringBuilder("Hello123$World!");

for (int i = sb.Length - 1; i >= 0; i--)
{
    if (Char.IsNumber(sb[i]) || sb[i] == '$')
    {
        sb.Remove(i, 1);
    }
}

Console.WriteLine(sb.ToString()); // output: HelloWorld
```

In this example, we use a for loop to iterate through the characters in the string and check if they are numbers or the dollar symbol. If they are, we use the Remove() method from the StringBuilder class to delete the character at that specific index. This method takes in two parameters: the index to start removing characters and the number of characters to remove. 

## Deep Dive
In both of the above examples, we used a regular expression pattern to match the characters we wanted to delete. Regular expressions, also known as regex, are powerful tools for pattern matching in strings. They allow us to specify a set of rules to search for specific patterns in a string and manipulate them accordingly. 

Regular expressions consist of characters that represent certain behavior, such as searching for specific characters, numbers, or symbols. In our examples, we used the `[0-9$]` pattern to match all numbers and the dollar symbol. This is just one of the many patterns you can use in regex. 

To learn more about regular expressions and how to use them in C#, check out the following resources:

- [C# Regular Expressions Basics](https://www.c-sharpcorner.com/UploadFile/8a67c0/regular-expression-in-C-Sharp/)
- [Regular Expressions in C# Tutorial](https://www.tutorialspoint.com/csharp/csharp_regular_expressions.htm)

## See Also
- [StringBuilder Class in C#](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-5.0)
- [Regex Class in C#](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=net-5.0)