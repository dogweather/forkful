---
date: 2024-01-20 17:41:13.845892-07:00
description: "How to: Pliki tymczasowe s\u0105 stare jak sama informatyka. S\u0142\
  u\u017C\u0105 do wielu cel\xF3w, od buforowania i przetwarzania danych po testy\
  \ jednostkowe bez ryzyka\u2026"
lastmod: '2024-04-05T22:50:49.977866-06:00'
model: gpt-4-1106-preview
summary: "Pliki tymczasowe s\u0105 stare jak sama informatyka."
title: Tworzenie pliku tymczasowego
weight: 21
---

## How to:
```PowerShell
# Tworzenie pliku tymczasowego
$tempFile = [System.IO.Path]::GetTempFileName()
"Przykładowa zawartość" | Out-File -FilePath $tempFile

# Wyświetlenie ścieżki do pliku tymczasowego
$tempFile

# Odczytanie i wyświetlenie zawartości pliku
Get-Content -Path $tempFile

# Usunięcie pliku tymczasowego
Remove-Item -Path $tempFile
```

Sample output:
```
C:\Users\Example\AppData\Local\Temp\tmp1234.tmp
Przykładowa zawartość
```

## Deep Dive
Pliki tymczasowe są stare jak sama informatyka. Służą do wielu celów, od buforowania i przetwarzania danych po testy jednostkowe bez ryzyka uszkodzenia stałych danych. W PowerShellu, `[System.IO.Path]::GetTempFileName()` wykorzystuje API Windowsa do zapewnienia unikatowej nazwy pliku, który jest automatycznie tworzony w folderze tymczasowym systemu. Można też stworzyć plik tymczasowy ręcznie, używając cmdlet `New-Item`, ale GetTempFileName() jest prostsze i bezpieczniejsze, bo minimalizuje ryzyko konfliktów nazw.

Alternatywą mogą być także niestandardowe skrypty używające `Get-Random` do stworzenia unikalnych nazw plików, ale to mniej polecane z uwagi na potencjalne kolizje nazw. W zakresie implementacji warto pamiętać, by zawsze usuwać pliki tymczasowe po ich użyciu, aby nie zaśmiecać systemu zbędnymi plikami.

## See Also
- [File and Stream I/O](https://docs.microsoft.com/en-us/dotnet/standard/io/)
- [PowerShell documentation](https://docs.microsoft.com/en-us/powershell/)
