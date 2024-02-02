---
title:                "Converting a string to lower case"
date:                  2024-02-01T13:31:30.107925-07:00
model:                 gpt-4-0125-preview
simple_title:         "Converting a string to lower case"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
So, you want to turn a string into all lowercase letters, huh? This trick is super handy when you're trying to compare strings without worrying about pesky case differences or for uniformity when saving and displaying text data. 

## How to:
In VBA (Visual Basic for Applications), making a string all lowercase is a piece of cake. You use the `LCase` function. Here's how it works:

```basic
Sub ConvertStringToLowercase()
    Dim originalString As String
    Dim lowercaseString As String
    
    originalString = "HeLLo WoRLd!"
    lowercaseString = LCase(originalString)
    
    MsgBox "Original: " & originalString & vbCrLf & "Lowercase: " & lowercaseString
End Sub
```

This code snippet defines a string, converts it to lowercase, and then pops up a message box showing both the original and the converted string. Run this, and you’ll see:

```
Original: HeLLo WoRLd!
Lowercase: hello world!
```

Pretty straightforward, right?

## Deep Dive
The `LCase` function has been a part of VBA for ages, dating back to its early versions. It's there to ensure that you can easily manipulate string cases without getting into the weeds of ASCII values or writing your own functions. Under the hood, `LCase` converts each character in your string to its lowercase equivalent based on the ASCII standard, doing all the heavy lifting for you.

While `LCase` is perfect for basic case conversion, there are scenarios where it might not meet all your needs, especially with international characters. In such cases, consider looking into newer technologies or libraries that can handle Unicode more gracefully. Nevertheless, for quick, simple conversions within the scope of English and similar languages, `LCase` in VBA does the job efficiently and without fuss.
