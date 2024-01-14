---
title:    "C# recipe: Using regular expressions"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Why

Have you ever found yourself searching for a specific pattern in a large text document? Maybe you needed to find all email addresses or phone numbers within a document. This is where regular expressions come in handy. They allow you to search for specific patterns within a larger text, making tasks like data validation and text parsing much easier.

## How To

Regular expressions, also known as regex, are a sequence of characters that define a search pattern. They are commonly used in programming languages, such as C#, to search and manipulate strings. Let's take a look at some examples of how to use regular expressions in C#.

```C#
//Import the System.Text.RegularExpressions namespace
using System.Text.RegularExpressions;

//Create a regex pattern to find email addresses
Regex emailPattern = new Regex(@"[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}");

//Search for matches within a string
MatchCollection emailMatches = emailPattern.Matches("john.doe@example.com, jane.smith@example.com");

//Loop through the matches and print out the results
foreach (Match match in emailMatches)
{
    Console.WriteLine(match.Value);
}

//Output:
//john.doe@example.com
//jane.smith@example.com
```

In this example, we first import the `System.Text.RegularExpressions` namespace to use regular expressions. Then, we create a `Regex` object with the pattern we want to search for. In this case, we are searching for email addresses. Next, we use the `Matches()` method to find all matches within a given string and store them in a `MatchCollection` object. Finally, we loop through the matches and print out the results.

You can also use regular expressions for data validation. For example, if you want to make sure a user enters a valid phone number in a specific format, you can use a regular expression to check for it. 

```C#
//Create a regex pattern to validate phone numbers in the format of (123)456-7890
Regex phonePattern = new Regex(@"^\([0-9]{3}\)[0-9]{3}-[0-9]{4}$");

//Check if a given string matches the pattern
string input = Console.ReadLine();
bool isValid = phonePattern.IsMatch(input);

//Output:
//Enter a phone number: (123)456-7890
//True
```

## Deep Dive

Regular expressions may seem daunting at first, with all the special characters and rules, but once you understand the basics, they can be a powerful tool in your programming arsenal.

One important thing to note is that regular expressions are case sensitive. This means that using the `RegexOptions.IgnoreCase` option is useful to make your patterns case insensitive.

Another useful feature of regular expressions is the use of quantifiers. These characters allow you to specify the number of matches you want to find. For example, the `*` quantifier means "zero or more occurrences" while the `+` quantifier means "one or more occurrences". These quantifiers can be used to make your patterns more flexible and match varying lengths of strings.

Lastly, regular expressions can also be used for replacement, where you can replace certain patterns within a string with something else. This is done using the `Replace()` method and passing in the pattern, replacement string, and the original string as parameters.

## See Also

- [Microsoft Docs: Regular Expression Language - Quick Reference](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [Regex Tester](https://regexr.com/)