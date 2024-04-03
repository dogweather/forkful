---
date: 2024-01-20 17:47:46.919728-07:00
description: "In PowerShell, finding the length of a string means to count the number\
  \ of characters it contains. Programmers do it to validate input, manipulate text\u2026"
lastmod: '2024-03-13T22:45:00.274038-06:00'
model: gpt-4-1106-preview
summary: In PowerShell, finding the length of a string means to count the number of
  characters it contains.
title: Finding the length of a string
weight: 7
---

## How to:
PowerShell makes getting the string length straightforward. Just toss a string at the `.Length` property, like this:

```PowerShell
$myString = "Hello, World!"
$myStringLength = $myString.Length
Write-Host "The string length is: $myStringLength"
```

You'll get the output:

```
The string length is: 13
```

That's all there is to it. Direct and painless.

## Deep Dive
Back in the day, getting the length of a string in most programming languages involved complex functions or processes. Today, it’s as simple as a property call in PowerShell.

Beyond the basic `.Length` property, PowerShell doesn't offer built-in alternatives for this specific task. However, before PowerShell became a thing, scripting in Windows was done via batch files or VBScript, where finding a string length was not as straightforward.

In terms of implementation, when you use `$myString.Length`, PowerShell accesses the metadata of the string object – strings in PowerShell are objects from the System.String class, which comes from .NET. The `.Length` property is a member of that class.

## See Also
Dive deeper into PowerShell strings:

For broader context on how strings work in .NET:
- [String Class in .NET](https://docs.microsoft.com/dotnet/api/system.string)
- [String.Length Property in .NET](https://docs.microsoft.com/dotnet/api/system.string.length)
