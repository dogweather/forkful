---
title:                "C# recipe: Searching and replacing text"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself needing to make a change to a large amount of text, but manually editing each instance would take too much time? That's where searching and replacing text comes in handy. This useful tool allows you to quickly and efficiently make changes to multiple instances of text at once, saving you valuable time and effort.

## How To

To search and replace text in C#, we use the `Replace()` method. This method takes in two string parameters: the text to be replaced and the text to replace it with. Let's take a look at an example:

```C#
string originalText = "Hello World!";
string modifiedText = originalText.Replace("World", "Universe");
Console.WriteLine(modifiedText);
```

The above code will output "Hello Universe!" as it has replaced the word "World" with "Universe" in the original text. You can also use this method to replace multiple instances of a word or phrase, making it even more efficient.

```C#
string originalText = "I love apples, apples are the best!";
string modifiedText = originalText.Replace("apples", "bananas");
Console.WriteLine(modifiedText);
```

The output in this case would be "I love bananas, bananas are the best!" as both instances of "apples" have been replaced with "bananas". This method is not case-sensitive, so it will replace all instances regardless of capitalization.

## Deep Dive

It's important to note that the `Replace()` method returns a new string with the modifications, leaving the original string unchanged. This is because strings in C# are immutable, meaning they cannot be changed once created. The `Replace()` method allows us to work around this by creating a new string with our desired modifications.

If you need to replace text in a specific location within a string, you can use the overload of the `Replace()` method that accepts a third parameter, which specifies the starting index of the replacement. This allows you to target specific instances of the text to be replaced.

Searching and replacing text can also be done using regular expressions, which allow for more complex and flexible pattern matching. This is especially useful when you need to replace text based on a specific pattern or condition. C# has a `Regex.Replace()` method for this purpose.

## See Also

For more information on the `Replace()` method and regular expressions, check out the following resources:

- [Microsoft Docs - String.Replace Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0)
- [Microsoft Docs - Regular Expressions in .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Tutorialspoint - C# String Replace Method](https://www.tutorialspoint.com/csharp/csharp_string_replace.htm)