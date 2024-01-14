---
title:                "C# recipe: Using regular expressions"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

##Why

Have you ever found yourself searching through a large text document for a specific word or phrase? Or maybe you need to validate user input in your program? Regular expressions, or simply "regex", provides a powerful and efficient way to search, match, and manipulate text. It's a must-have tool for any developer looking to save time and increase productivity.

##How To

Using regular expressions in your C# code is relatively straightforward. First, you'll need to add the namespace "System.Text.RegularExpressions" to your code. This provides access to the Regex class, which is responsible for performing regex operations.

Next, create an instance of the Regex class by passing in the pattern you want to match as a string. For example, to find all words that start with the letter "s" in a given string, you can use the pattern "s\w+" (the backslash escapes the special character "\w"). Then, use the "Match" method to find all the matches in your string.

```
using System.Text.RegularExpressions;

//create a regex instance
Regex regex = new Regex("s\w+");

//find all matches in a string
MatchCollection matches = regex.Matches("Sally sells seashells by the seashore.");

//loop through each match and print the result
foreach (Match match in matches)
{
    Console.WriteLine(match.Value); //prints "sells" and "seashells"
}
```

You can also use regular expressions to replace text. The "Replace" method takes in three parameters: the input string, the replacement string, and an optional integer to specify the maximum number of replacements. For instance, to replace all instances of the word "red" with "blue" in a given string, you can use the following code:

```
string newString = Regex.Replace("I have a red car and a red bike.", "red", "blue");
//newString will now be "I have a blue car and a blue bike."
```

Regular expressions also allow for advanced patterns such as character classes, quantifiers, and backreferences. It may seem daunting at first, but with practice, you'll be able to create complex regex patterns to suit your needs. For more in-depth tutorials on using regular expressions in C#, MSDN and Regex101 are great resources to explore.

##Deep Dive

One thing to keep in mind when using regular expressions in C# is that they are case-sensitive by default. If you want to perform a case-insensitive search, you can use the "IgnoreCase" option in the Regex constructor. You can also use the "RegexOptions" enum to specify other options such as "Singleline" or "Multiline", which can affect how the regex pattern matches text.

Another important concept to understand is the concept of "greedy" and "lazy" matching. By default, regular expressions are greedy, meaning they will match as much text as possible. For example, the pattern "a.+" will match "apple" in the string "I have an apple pie" instead of just the first letter "a". To make the matching lazy, simply add a question mark after the quantifier (e.g "a.+?").

It's also worth noting that regex can be resource-intensive and if overused, can significantly impact the performance of your code. So, it's important to understand when and where to use regular expressions and to consider alternative approaches if necessary.

##See Also

- [MSDN - Regular Expression Language](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Regex101 - Interactive Regex Tutorial and Debugger](https://regex101.com/)
- [DotNetPerls - C# Regular Expressions Guide](https://www.dotnetperls.com/regex)