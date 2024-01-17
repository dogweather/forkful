---
title:                "Reading a text file"
html_title:           "PowerShell recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file means accessing and retrieving data from a file that contains plain text. Programmers often do this to extract specific information or manipulate the data for a desired output.

## How to:

Reading a text file in PowerShell is a simple process that can be done in just a few lines of code. Here are some examples:

```
# Reading a text file and displaying the contents
$text = Get-Content C:\Users\Username\Documents\sample.txt
$text

# Reading a text file and storing the contents in a variable
$contents = Get-Content C:\Users\Username\Documents\sample.txt

# Reading a specific number of lines from a text file
$lines = Get-Content C:\Users\Username\Documents\sample.txt -TotalCount 10
```

#### Sample Output:

```
This is a sample text file.

It contains multiple lines of plain text.

Reading this file in PowerShell is a breeze.
```

## Deep Dive:

Reading text files is a fundamental task in programming, dating back to the earliest days of computing. Before graphical user interfaces (GUIs) were developed, text files were the primary means of storing and exchanging data. Today, alternative methods such as reading and parsing XML or JSON files may be more common, but reading text files remains a crucial skill for any programmer.

In PowerShell, the `Get-Content` cmdlet is used to read a text file. It can also be used to read and manipulate the contents of other types of files, such as CSV, XML, and JSON.

One important note is that the `Get-Content` cmdlet is case-sensitive. This means that if the file name or path is not entered correctly, the cmdlet will not be able to find the file and an error will be returned.

## See Also:

For more information on reading text files in PowerShell, check out the official Microsoft documentation: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7

To learn more about the `Get-Content` cmdlet and its usage, you can also view the help documentation by running `Get-Help Get-Content` in your PowerShell console.