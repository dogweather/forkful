---
title:    "C# recipe: Deleting characters matching a pattern"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

As programmers, we often encounter situations where we need to manipulate strings in our code. One common task is to delete characters that match a specific pattern. This can be useful for tasks such as data cleansing or formatting output. In this blog post, we will explore how to delete characters matching a pattern in C#.

## How To

To delete characters matching a pattern in C#, we can use the `Regex.Replace()` method from the `System.Text.RegularExpressions` namespace. This method allows us to specify a regular expression pattern to identify the characters we want to delete and replace them with an empty string. Let's look at an example:

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main(string[] args)
    {
        string str = "Hello123World";
        string pattern = "[0-9]"; // this pattern will match any digit

        string result = Regex.Replace(str, pattern, "");

        Console.WriteLine(result); // output: HelloWorld
    }
}
```

In this example, we first declare a string variable `str` with the value "Hello123World". This string contains some numbers that we want to get rid of. Next, we declare a string variable `pattern` that contains our regular expression pattern. The pattern `[0-9]` specifies that we want to match any digit. Finally, we use the `Regex.Replace()` method to replace any characters in `str` that match our pattern with an empty string. The resulting string is assigned to the `result` variable and printed to the console.

We can also use the `Regex.Replace()` method with more complex patterns and multiple replacements. For example, let's say we want to delete all numbers, special characters, and spaces from a string. We can use the following code:

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main(string[] args)
    {
        string str = "H3ll0 W0rld!@";
        string pattern = "[0-9!@ ]"; // this pattern will match any digit, !, @ and spaces

        string result = Regex.Replace(str, pattern, "");

        Console.WriteLine(result); // output: HelloWorld
    }
}
```

In this example, we are using the character class `[0-9!@ ]` in our pattern which will match any digits, exclamation marks, @ symbols, and spaces. The result is the same as our previous example, but this time we are removing multiple types of characters.

## Deep Dive

When using regular expressions to delete characters matching a pattern, it is important to understand the syntax and rules behind them. Regular expressions allow us to specify a pattern of characters that we want to match in a string. The `[]` character class that we used in our examples allows us to specify a range of characters or character classes that we want to match. We can also use metacharacters such as `.` or `^` to match any character or the beginning of a line, respectively.

Regular expressions can get quite complex, so it's important to test and debug our patterns to ensure they are matching the characters we want. Online tools such as [Regex101](https://regex101.com/) can be useful in this process.

## See Also

- Microsoft Docs on [Regex.Replace() Method](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace?view=net-5.0)
- [Regular Expression Tutorial](https://www.regular-expressions.info/tutorial.html)