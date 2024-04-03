---
date: 2024-01-20 17:35:42.391730-07:00
description: "Combining strings, or concatenation, is like making a train of words.\
  \ We do it to stitch together text values, creating phrases, sentences, or anything\u2026"
lastmod: '2024-03-13T22:45:00.274885-06:00'
model: gpt-4-1106-preview
summary: Combining strings, or concatenation, is like making a train of words.
title: Concatenating strings
weight: 3
---

## What & Why?
Combining strings, or concatenation, is like making a train of words. We do it to stitch together text values, creating phrases, sentences, or anything where separate strings need to buddy up and become one.

## How to:
Let's get straight to it:

```PowerShell
# Using the '+' operator
$greeting = 'Hello, ' + 'World!'
$greeting # Outputs: Hello, World!

# Via string interpolation
$name = 'Jane'
$welcomeMessage = "Hi, $name, nice to meet you!"
$welcomeMessage # Outputs: Hi, Jane, nice to meet you!

# With the -f operator (format operator)
$city = 'New York'
$visitMessage = 'Welcome to {0}!' -f $city
$visitMessage # Outputs: Welcome to New York!

# StringBuilder for complex scenarios (a bit overkill for simple stuff)
$textBuilder = New-Object System.Text.StringBuilder
[void]$textBuilder.Append('PowerShell ')
[void]$textBuilder.Append('is ')
[void]$textBuilder.Append('awesome.')
$textBuilder.ToString() # Outputs: PowerShell is awesome.
```

## Deep Dive
Historically, concatenating strings was a bit rough around the edges in earlier programming languages - think of it like using tape to stick sentences together. In PowerShell, it’s a walk in the park.

There are different ways to get the job done. The '+' operator is straightforward but can be slow with lots of strings. String interpolation with "$variable" is cleaner, and great for inserting variables into strings. The format operator '-f' shines in templating scenarios.

About performance - if you're combining an essay's worth of strings, you'll want something more heavy-duty. Enter `StringBuilder`. It doesn't concatenate immediately; instead, it weaves your strings together when summoned, saving time and memory for big concatenation tasks.

## See Also
- [About Join](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_join?view=powershell-7.3)
- [About Automatic Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.3) (see `$OFS`)
- For more on string formatting, check out [Composite Formatting](https://docs.microsoft.com/en-us/dotnet/standard/base-types/composite-formatting).
- And, if you've got the stomach for it, here's the nitty-gritty on [StringBuilder](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-6.0).
