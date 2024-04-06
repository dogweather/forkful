---
date: 2024-01-27 16:14:27.151355-07:00
description: "How to: Let's start with a simple task: you want to replace all instances\
  \ of \"oldtext\" with \"newtext\" in a file named example.txt. Here's how you would\
  \ do\u2026"
lastmod: '2024-03-13T22:45:00.280321-06:00'
model: gpt-4-0125-preview
summary: Let's start with a simple task.
title: Editing files in-place with CLI one-liners
weight: 32
---

## How to:


### Replacing Text in a Single File
Let's start with a simple task: you want to replace all instances of "oldtext" with "newtext" in a file named example.txt. Here's how you would do it:

```PowerShell
(Get-Content example.txt) -replace 'oldtext', 'newtext' | Set-Content example.txt
```

This one-liner reads the content, performs the replacement, and writes the content back to the original file.

### Editing Multiple Files
What if you need to apply the same change across multiple files? Here's an approach using a loop:

```PowerShell
Get-ChildItem *.txt | ForEach-Object {
  (Get-Content $_) -replace 'oldtext', 'newtext' | Set-Content $_
}
```

This snippet finds all `.txt` files in the current directory, replacing "oldtext" with "newtext" in each one.

### Add Content at the Beginning or End of Files
Appending or prepending content can also be streamlined:

```PowerShell
# Prepending
"New first line`n" + (Get-Content example.txt) | Set-Content example.txt

# Appending
(Get-Content example.txt) + "`nNew last line" | Set-Content example.txt
```

Here, we simply concatenate the new content before or after the existing content and save it back.

## Deep Dive
Historically, in-place editing is more commonly associated with Unix tools like `sed` and `awk`. PowerShell, being a more recent entrant, does not include a dedicated in-place editing feature out of the box. This is partly due to its design philosophy, highlighting the importance of objects over text streams, unlike Unix tools that treat most inputs as text.

Alternatives to PowerShell for this task include using traditional Unix tools available on Windows through Cygwin or the Windows Subsystem for Linux (WSL). These tools often provide a more concise syntax for in-place editing due to their text-centric design.

Implementation-wise, it's important to note that PowerShell's approach involves reading the entire file into memory, making changes, and then writing it back. While this works well for moderately sized files, it can become inefficient for very large files. In such cases, one might consider using `.NET` methods directly or resorting to alternative tools designed for streaming large volumes of data.

Despite these considerations, PowerShell's flexibility and extensive feature set make it an invaluable tool for manipulating files directly from the command line, especially for those already entrenched in the Windows ecosystem or managing cross-platform environments.
