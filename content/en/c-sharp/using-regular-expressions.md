---
title:    "C# recipe: Using regular expressions"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why
Regular expressions are a powerful tool in the world of programming. They allow us to search, manipulate, and validate strings of text with ease. By learning how to use regular expressions, you can streamline your code and make it more efficient.

## How To
Using regular expressions in C# is actually quite simple. Let's look at a few examples to see how they work.

```C#
// Match a specific word in a sentence
Regex regex = new Regex("hello");
string sentence = "Hello there, how are you?";
Match match = regex.Match(sentence);
Console.WriteLine(match); // Output: "Hello"

// Find multiple occurrences
Regex regex = new Regex("the");
string sentence = "The cat in the hat";
MatchCollection matches = regex.Matches(sentence);
foreach (var match in matches)
{
    Console.WriteLine(match); // Output: "the" "the"
}

// Replace a word in a sentence
Regex regex = new Regex("apple");
string sentence = "I love apples!";
string newSentence = regex.Replace(sentence, "oranges");
Console.WriteLine(newSentence); // Output: "I love oranges!"

// Validate email address
Regex regex = new Regex(@"\w+@\w+\.\w+");
string email = "johndoe@email.com";
if (regex.IsMatch(email))
{
    Console.WriteLine("Valid email address");
}
else
{
    Console.WriteLine("Invalid email address");
}
// Output: Valid email address
```

## Deep Dive
Regular expressions may seem like a random assortment of symbols and characters, but each one serves a specific purpose. For example, the backslash "\" symbol indicates an escape character, which is used to give special meaning to certain characters. The dot "." symbol is a wildcard that represents any single character. By learning the different symbols and their meanings, you can create complex regular expressions to suit your needs.

Other advanced features of regular expressions in C# include capturing groups, which allow you to retrieve specific parts of a match, and lookaround assertions, which let you specify the position of a match relative to another string. These features can greatly enhance the functionality and efficiency of your regular expressions.

## See Also
- [Regular Expressions in C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions-in-csharp)
- [Regular Expressions 101](https://regex101.com/)
- [Mastering Regular Expressions by Jeffrey Friedl](https://www.amazon.com/Mastering-Regular-Expressions-Jeffrey-Friedl/dp/0596528124)