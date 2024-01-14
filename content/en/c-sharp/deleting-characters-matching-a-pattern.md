---
title:                "C# recipe: Deleting characters matching a pattern"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why
Have you ever had a text file with thousands of lines and you needed to delete certain characters that matched a specific pattern? Or perhaps you've encountered a situation where you needed to clean up a database by removing certain characters from a particular column. Whatever the case may be, being able to efficiently delete characters matching a pattern can save you a lot of time and effort.

## How To
To start off, let's take a look at a simple example where we have a string with numbers and symbols and we want to remove the symbols. Our string looks like this: "12#3%45&".
```C#
string myString = "12#3%45&";
string result = Regex.Replace(myString, "[^0-9]", "");
Console.WriteLine(result);
```
The output of this code will be "12345", which is our desired result. Let's break down the code a bit. The first line creates a string variable with our original string. The second line uses the Regex.Replace method to replace any character that is not a number (represented by the regular expression [^0-9]) with an empty string. This essentially removes all symbols from the string. Finally, the third line prints out the updated string.

But what if we want to remove a specific character instead of a pattern? We can do that too! Let's say we want to remove all the letter "a" from a string. Here's an example code:
```C#
string myString = "apple";
string result = myString.Replace("a", "");
Console.WriteLine(result);
```
The output will be "pple" as all instances of "a" have been removed. Notice that we used the Replace method instead of Regex.Replace this time.

## Deep Dive
Now, let's take a deeper look into how these methods work. The Regex.Replace method uses regular expressions to find and replace characters that match a specific pattern. This gives us more flexibility and allows us to delete multiple characters at once. On the other hand, the Replace method simply looks for an exact match and replaces it with the specified value.

It's also important to note that both methods return a new string instead of modifying the original string. This means that if we want to keep the original string, we need to assign the updated string to a new variable, as shown in our code examples above.

Another useful thing to know is that these methods are case sensitive. So if we want to match and remove both upper and lowercase versions of a character, we need to specify that in our regular expression.

## See Also
- [C# Strings](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [C# Regular Expressions](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)

And that's it! You now know how to efficiently delete characters matching a pattern in C#.