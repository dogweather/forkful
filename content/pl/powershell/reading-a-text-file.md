---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Czytanie plików tekstowych w PowerShell - Jak to zrobić?

## Co & Dlaczego?
Czytanie z plików tekstowych to proces, w którym wskaźnik wczytuje dane z pliku tekstowego do pamięci programu. Programiści robią to, aby móc przetwarzać zapisane wcześniej dane, których używają ich programy.

## Jak to zrobić:
Używać będziemy cmdlet `Get-Content`. Przykładowe użycie:

```PowerShell
$tekst = Get-Content -Path C:\mojPlik.txt
Write-Output $tekst
```

Jeżeli `mojPlik.txt` zawierałby tekst "Witaj, świecie!", to output wyglądałby tak:

```PowerShell
Witaj, świecie!
```

## Deep Dive
Cmdlet `Get-Content` to jeden z najwcześniejszych sposobów na odczyt plików tekstowych wprowadzonych w PowerShell. Alternatywą może być funkcja [.NET](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readalltext?view=net-5.0), takie jak `System.IO.File]::ReadAllText("C:\mojPlik.txt")`. Pod względem wykonania, wydajność cmdlet `Get-Content` jest lepsza przy dużych plikach, ponieważ odczytuje plik sekwencyjnie, podczas gdy `ReadAllText` wczytuje cały plik do pamięci na raz.

## Zobacz też:

- [Dokumentacja Microsoft dla Get-Content](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1)
- [Porównanie wydajności cmdlet 'Get-Content'](https://powershell.org/2013/10/the-get-content-cmdlet/)
- [Wiecej o System.IO.File]::ReadAllText](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readalltext?view=net-5.0)