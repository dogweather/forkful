---
title:                "Reading a text file"
date:                  2024-02-01T13:31:46.481336-07:00
model:                 gpt-4-0125-preview
simple_title:         "Reading a text file"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/vba/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Reading a text file in VBA is about grabbing all that sweet text data from, well, text files into your program to do with it as you please. Programmers do it because, let's face it, a ton of valuable data and info comes in plain old text files, and we need a way to access and manipulate that data without going bananas.

## How to:
Alright, diving straight into the good stuff, here's how you pull off this magic trick in VBA. Say you got a file, "example.txt", lounging somewhere on your computer. You want to read it. Here’s how you're gonna do it:

```basic
Sub ReadTextFile()
    Dim filePath As String
    Dim fileContent As String
    Dim fileNum As Integer
    
    filePath = "C:\path\to\your\example.txt"  ' Change this to your file's path
    
    fileNum = FreeFile()  ' Get a free file number
    Open filePath For Input As #fileNum  ' Open your file for reading
    
    Do While Not EOF(fileNum)  ' Loop until the end of the file
        Line Input #fileNum, fileContent  ' Read a line from the file
        Debug.Print fileContent  ' Print the line to the Immediate Window
    Loop
    
    Close #fileNum  ' Always close your file when done
End Sub
```

Run this sub, and you'll see the content of "example.txt" merry-go-rounding in the Immediate Window of the VBA editor. Neat, huh?

## Deep Dive
Diving a bit deeper, the method shown uses the classic `Open` statement combined with a file number to access the text file. This shebang has been around since the stone age of BASIC, giving it a somewhat nostalgic, if not a bit archaic, vibe. It's solid for simple tasks but comes with its quirks, like managing file numbers and manually handling file opening and closing.

In the modern world outside VBA, programmers often favor more straightforward methods with automatic resource management and support for more complex data formats (JSON, XML, you name it). For instance, Python with its elegant `with open('file') as file:` syntax.

Nonetheless, when you're locked into VBA, whether by choice or by the cruel twists of corporate mandates, mastering file I/O operations like these is essential. Plus, it’s a neat party trick to pull out when you need to impress someone with your macro mastery or just get some actual work done. So there you have it, the down and dirty on reading text files in VBA.
