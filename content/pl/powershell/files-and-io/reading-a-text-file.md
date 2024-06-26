---
date: 2024-01-20 17:54:56.114422-07:00
description: "How to: (Jak to zrobi\u0107:) Wyj\u015Bcie przyk\u0142adowe."
lastmod: '2024-04-05T21:53:37.072515-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Wyj\u015Bcie przyk\u0142adowe."
title: Odczytywanie pliku tekstowego
weight: 22
---

## How to: (Jak to zrobić:)
```PowerShell
# Wczytanie całego pliku
$content = Get-Content -Path 'C:\plik.txt'
Write-Host $content

# Wczytanie pliku linia po linii
Get-Content -Path 'C:\plik.txt' | ForEach-Object {
    Write-Host $_
}
```

Wyjście przykładowe:
```
To jest zawartość pliku tekstowego.
Druga linia tekstu.
```

## Deep Dive (Głębsze spojrzenie)
Czytanie plików tekstowych jest tak stare jak pierwsze komputery. Alternatywami `Get-Content` są `[System.IO.File]::ReadAllText('ścieżka')` w .NET, czy `cat` w Unixowych skryptach shell'a. A przy dużych plikach? Użyj `-ReadCount` i `-TotalCount` w `Get-Content` dla efektywności.

Implementacja `Get-Content` korzysta z Windows Management Instrumentation (WMI), dając dostęp do wielu zaawansowanych funkcji, m.in. kodowania czy przesyłania strumieniowego.

## See Also (Zobacz również)
- [Microsoft Official Documentation for Get-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1)
- [About_Automatic_Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.1#_)
- [StackOverflow: Reading large text files with Powershell](https://stackoverflow.com/questions/21737976/powershell-read-large-log-files)
