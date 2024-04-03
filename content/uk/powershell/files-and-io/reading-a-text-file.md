---
date: 2024-01-20 17:55:05.340339-07:00
description: "\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438: \u0427\u0438\u0442\
  \u0430\u043D\u043D\u044F \u0432\u0441\u044C\u043E\u0433\u043E \u0444\u0430\u0439\
  \u043B\u0443 \u043E\u0434\u0440\u0430\u0437\u0443."
lastmod: '2024-03-13T22:44:49.679450-06:00'
model: gpt-4-1106-preview
summary: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0432\u0441\u044C\u043E\u0433\
  \u043E \u0444\u0430\u0439\u043B\u0443 \u043E\u0434\u0440\u0430\u0437\u0443."
title: "\u0427\u0438\u0442\u0430\u043D\u043D\u044F \u0442\u0435\u043A\u0441\u0442\u043E\
  \u0432\u043E\u0433\u043E \u0444\u0430\u0439\u043B\u0443"
weight: 22
---

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
