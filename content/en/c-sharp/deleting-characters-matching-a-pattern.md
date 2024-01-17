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

## What & Why?

Deleting characters matching a pattern is a common task in programming, where we want to remove specific characters or sets of characters from a given string. This is useful for cleaning up data, removing unwanted characters, or manipulating the text for certain operations. Programmers use this to make their code more efficient and to ensure the desired output is achieved without any unnecessary characters.

## How to:

To delete characters matching a pattern in C#, we can use the `string.Replace()` method. This method takes in two parameters - the first being the character or set of characters we want to replace, and the second being the replacement character or string. Let's take a look at an example:

```
string text = "Hello World!";
text = text.Replace("o", "");
Console.WriteLine(text); // Output: Hell Wrld!
```

In this example, we are using the `string.Replace()` method to remove the character `o` from the string `Hello World!`. We can see that the character `o` is replaced with an empty string, effectively removing it from our original string.

We can also use this method to remove multiple characters by providing a longer string as the first parameter. For example:

```
string text = "Hello World!";
text = text.Replace("o", "").Replace("l", "");
Console.WriteLine(text); // Output: He Wrddd!
```

In this case, both the characters `o` and `l` are replaced with an empty string, resulting in the removal of `o` and `l` from our original string.

## Deep Dive:

Deleting characters matching a pattern has been a common task in programming since the early days of computer programming. In the past, this was often done through manual string manipulation, where characters were individually checked and removed if they matched the desired pattern. However, with the advancements in programming languages and libraries, this task has become much easier and more efficient.

Apart from using the `string.Replace()` method, there are also other ways to delete characters matching a pattern in C#. One alternative is to use regular expressions, which allow for more complex patterns to be matched and removed from a string. This can be useful for cases where we want to remove specific types of characters, such as symbols or numbers, from a string.

When implementing the `string.Replace()` method, it's important to note that it is case-sensitive. This means that the characters we want to replace must match the case of the characters in the original string. For example, if we want to remove the letter `o` from the string `Hello World!`, using `string.Replace("o", "")` will only remove the lowercase `o` and not the uppercase `O`. To remove both cases of the letter `o`, we can use the `string.Replace()` method twice, once for each case.

## See Also:

To learn more about the `string.Replace()` method and its variations, you can refer to the official documentation: 
- https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0
- https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-5.0#System_String_Replace_System_String_System_String_