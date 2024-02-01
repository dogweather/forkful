---
title:                "Capitalizing a string"
date:                  2024-02-01T13:32:35.016689-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizing a string"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means changing the first character of each word to uppercase and the rest to lowercase. Programmers do this to ensure consistency in user inputs or to meet formatting requirements for displaying text.

## How to:
Capitalizing a string in VBA is straightforward thanks to the built-in `StrConv` function, which can be used to convert text to Proper Case, achieving the capitalization effect. Here’s how you do it:

```Visual Basic for Applications
Sub CapitalizeString()
    Dim exampleString As String
    exampleString = "hello world"
    ' Capitalize the string
    Dim capitalizedString As String
    capitalizedString = StrConv(exampleString, vbProperCase)
    ' Output: "Hello World"
    Debug.Print capitalizedString
End Sub
```

This code snippet takes a lowercase string "hello world" and converts each word's first character to uppercase. The result, "Hello World", is then printed to the Immediate Window in the VBA editor.

## Deep Dive
The `StrConv` function has been a part of VBA for quite some time, providing an easy way to change text case among other transformations like converting to Unicode or from Unicode. Specifically, the `vbProperCase` argument is used to capitalize strings as we've seen. While this approach is suitable for many cases, it's worth noting that `StrConv` may not correctly handle capitalization for certain locale-specific characters or complex cases like acronyms (e.g., converting "NASA" to "Nasa"). In applications where precise control over casing is required, custom functions or additional logic may be needed to handle exceptions. However, for general purposes, `StrConv` with `vbProperCase` offers a simple and effective solution for capitalizing strings in VBA.
