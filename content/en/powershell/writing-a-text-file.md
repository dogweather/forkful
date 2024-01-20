---
title:                "Writing a text file"
html_title:           "PowerShell recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Writing a text file refers to creating a file that stores text-based data, such as letters, numbers, or punctuation. Programmers often write text files as a way to store and retrieve data in an organized manner. 

## How to:
To write a text file using PowerShell, follow these steps:

1. Start by opening PowerShell on your computer. You can do this by typing "PowerShell" in the search bar or by navigating to the start menu and selecting "PowerShell". 

2. Next, use the `New-Item` cmdlet to create a new file in your desired location. In this example, we will create a new text file named "myFile.txt" in the Documents folder.

```PowerShell
New-Item -Path C:\Users\Username\Documents\myFile.txt -ItemType File
```

3. Now that we have our file created, we can use the `Set-Content` cmdlet to write text to the file. In the example below, we will write the sentence "Hello world!" to our file.

```PowerShell
Set-Content -Path C:\Users\Username\Documents\myFile.txt -Value "Hello world!"
```

4. To verify that our text was successfully written to the file, we can use the `Get-Content` cmdlet to retrieve the contents of the file.

```PowerShell
Get-Content -Path C:\Users\Username\Documents\myFile.txt
```

The output of the above command would be "Hello world!".

## Deep Dive:
Writing text files has been a common practice since the early days of computer programming. Text files were often used as a means of storing and transferring data between different computer systems.

Today, there are alternative methods for storing and manipulating data, such as databases, which provide more efficient ways of organizing and retrieving information. However, text files remain a popular choice for simple data storage and manipulation tasks.

When writing a text file in PowerShell, it is important to understand the different cmdlets that are available for working with files. `New-Item` allows us to create a new file, while `Set-Content` and `Get-Content` are used for writing and retrieving the contents of a file, respectively.

## See Also:
- [PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1)
- [Microsoft Dev Blogs - PowerShell](https://devblogs.microsoft.com/powershell/)