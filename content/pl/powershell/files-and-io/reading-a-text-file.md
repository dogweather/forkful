---
date: 2024-01-20 17:54:56.114422-07:00
description: "Czytanie pliku tekstowego to pobieranie jego zawarto\u015Bci do pami\u0119\
  ci dla dalszej obr\xF3bki. Programi\u015Bci robi\u0105 to codziennie, by np. wczytywa\u0107\
  \ konfiguracje,\u2026"
lastmod: '2024-03-13T22:44:35.647228-06:00'
model: gpt-4-1106-preview
summary: "Czytanie pliku tekstowego to pobieranie jego zawarto\u015Bci do pami\u0119\
  ci dla dalszej obr\xF3bki."
title: Odczytywanie pliku tekstowego
weight: 22
---

## What & Why? (Co i Dlaczego?)
Czytanie pliku tekstowego to pobieranie jego zawartości do pamięci dla dalszej obróbki. Programiści robią to codziennie, by np. wczytywać konfiguracje, analizować dane lub przetwarzać logi.

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
