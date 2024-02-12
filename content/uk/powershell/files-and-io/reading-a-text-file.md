---
title:                "Читання текстового файлу"
aliases:
- /uk/powershell/reading-a-text-file/
date:                  2024-01-20T17:55:05.340339-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання текстового файлу"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Що і чому?

Читання текстового файлу — це процес отримання даних із файлу, який зберігає текст. Програмісти роблять це для обробки, аналізу, або виведення інформації, збереженої у цих файлах.

## Як робити:

Читання всього файлу одразу:

```PowerShell
$content = Get-Content -Path "D:\Example\file.txt"
Write-Host $content
```

Sample output:

```
Це текст з файлу.
Ура!
```

Читання файлу построково:

```PowerShell
Get-Content -Path "D:\Example\file.txt" | ForEach-Object {
    Write-Host $_
}
```

Sample output:

```
Це текст з файлу.
Ура!
```

Використання `StreamReader` для великих файлів:

```PowerShell
$stream = [System.IO.StreamReader] "D:\Example\huge_file.txt"
while ($line = $stream.ReadLine()) {
    Write-Host $line
}
$stream.Close()
```

## Поглиблено

Reading text files is fundamental in programming. In the DOS era, we had `TYPE` and `MORE` commands. PowerShell, more advanced, introduced `Get-Content` cmdlet, which is part of the Microsoft.PowerShell.Management module. 

While `Get-Content` is handy for small-to-medium files, `StreamReader` (a .NET class) should be used for larger ones to save memory and improve performance. For instance, `StreamReader` reads a file lazily (line by line) when you loop through it, instead of loading the entire content into memory.

You can also use other .NET classes like `File.ReadAllLines` or `File.ReadAllText` when interoperating within different .NET languages, which further exemplifies the versatility in file handling within the PowerShell environment.

Alternatives within PowerShell include `Import-Csv` for structured data and `Select-String` for pattern matching, which both internally handle text file reading but with additional layers for their specific use-cases.

## Дивіться також

- [`Get-Content` documentation on Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content)
- [About StreamReaders on Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)
- [PowerShell subreddit - a community for questions and sharing scripts](https://www.reddit.com/r/PowerShell/)
