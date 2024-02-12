---
title:                "Odczytywanie pliku tekstowego"
aliases: - /pl/powershell/reading-a-text-file.md
date:                  2024-01-20T17:54:56.114422-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie pliku tekstowego"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

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
