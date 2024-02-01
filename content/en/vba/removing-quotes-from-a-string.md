---
title:                "Removing quotes from a string"
date:                  2024-02-01T13:31:35.174508-07:00
model:                 gpt-4-0125-preview
simple_title:         "Removing quotes from a string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Removing quotes from a string is about stripping away those annoying quotation marks that can mess up your data or output. Programmers do this to clean strings for display, further processing, or because sometimes data comes with extra quotes that just don't belong.

## How to:

In VBA, there are a couple of ways to remove quotes from a string, but let’s focus on a straightforward approach using the `Replace` function. This function simply searches for a substring within a string and replaces it with another substring. In this case, we're targeting both single (`'`) and double (`"`) quotes.

```Visual Basic for Applications
Sub RemoveQuotes()
    Dim exampleString As String
    
    'Original string with quotes
    exampleString = """Hello, 'World'!"""
    
    'Remove double quotes
    exampleString = Replace(exampleString, """", "")
    
    'Remove single quotes
    exampleString = Replace(exampleString, "'", "")
    
    'Output the cleaned string
    Debug.Print exampleString  'Output: Hello, World!
End Sub
```

## Deep Dive

The `Replace` function used here has been a part of VBA since its inception, offering a simple yet powerful way to manipulate strings. While removing quotes is a basic example, `Replace` can handle more complex patterns and replacements which can be crucial for data cleansing or preparation tasks. However, when dealing with more complex patterns, such as those involving variable positions or character sets, regular expressions (RegEx) might offer a more robust solution. Though not native to VBA (requiring a reference to Microsoft VBScript Regular Expressions 5.5), RegEx allows for pattern matching and replacement in strings, providing greater flexibility for complex string manipulation tasks beyond just removing quotes.
