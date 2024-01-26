---
title:                "Writing a text file"
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Writing a text file is saving data as plain text on disk. Programmers do it for logging, configuration, or storage of user data. It's basic but crucial for most applications.

## How to:
Here's how to write to a text file in PowerShell; it’s super easy!

Create and write text to a new file:
```PowerShell
"Hello, world!" | Out-File -FilePath .\hello.txt
```

Append text to an existing file:
```PowerShell
"Welcome to PowerShell scripting!" | Add-Content -Path .\hello.txt
```

Check the content of the file:
```PowerShell
Get-Content .\hello.txt
```

Sample output:
```
Hello, world!
Welcome to PowerShell scripting!
```

## Deep Dive
PowerShell files use UTF-16 encoding by default. Historically, text files were simpler—ASCII only. Now, `Out-File` and `Add-Content` let you choose the encoding. If you’re old-school, `Set-Content` exists but has limitations. For larger files, consider `[System.IO.StreamWriter]` for efficiency.

## See Also
For more PowerShell file-handling prowess, visit:
- Microsoft Docs on [Out-File](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/out-file)
- Microsoft Docs on [Add-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/add-content)
  
Remember, practice makes perfect. So get scripting!
