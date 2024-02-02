---
title:                "Using regular expressions"
date:                  2024-02-01T13:32:53.713291-07:00
model:                 gpt-4-0125-preview
simple_title:         "Using regular expressions"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular expressions, or regex, are a sequence of characters that form a search pattern. Programmers use them to match, search, and manipulate strings efficiently, making tasks like data validation or parsing complex text a breeze.

## How to:

To start using regular expressions in Visual Basic for Applications (VBA), you need to enable Microsoft VBScript Regular Expressions via the Tools -> References... in the VBA editor. Look for "Microsoft VBScript Regular Expressions 5.5" and check it. 

Let’s dive into a simple example:

```basic
Sub UseRegex()
    Dim regex As Object
    Set regex = CreateObject("VBScript.RegExp")

    With regex
        .Global = True
        .IgnoreCase = True
        .Pattern = "\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b" ' Regex pattern for email matching
    End With
    
    Dim testString As String
    testString = "Hello, my email is example@example.com. Feel free to reach out."
    
    If regex.Test(testString) Then
        MsgBox "Email found: " & regex.Execute(testString)(0)
    Else
        MsgBox "No email found."
    End If
End Sub
```
This code snippet sets up a regular expression object to match emails within a string. The `Global` and `IgnoreCase` properties allow for matching all instances case-insensitively. The `Test` method returns `True` if the pattern is found, and `Execute` extracts the matched value.

## Deep Dive

Regular expressions in VBA rely on the VBScript Regular Expressions engine, which has been around since VBScript itself. While this implementation provides a robust feature set for string manipulation, newer languages and platforms might offer more intuitive or powerful regex libraries, such as those found in Python or JavaScript. However, when working within the Office suite, utilizing VBA's access to the VBScript regex engine allows for sophisticated text processing directly inside applications like Excel or Access, enabling data analysts and other professionals to utilize the power of regex without leaving their familiar environment.
