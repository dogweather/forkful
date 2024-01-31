---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:15:57.594095-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
W skrócie, pobieranie bieżącej daty to nic innego jak sprawdzenie, jakie jest dzisiaj. Programiści często to robią, by logować zdarzenia, ustalać ramy czasowe czy też po prostu wyświetlać datę użytkownikom.

## How to:
Zobaczmy, jak to zrobić w PowerShell:

```PowerShell
# Pobranie i wyświetlenie aktualnej daty i czasu
Get-Date

# Formatowanie wyświetlanej daty
Get-Date -Format "yyyy-MM-dd HH:mm:ss"

# Zapisanie aktualnej daty do zmiennej
$currentDate = Get-Date
$currentDate
```

Sample output:

```
środa, 12 kwietnia 2023 14:05:46

2023-04-12 14:05:46

środa, 12 kwietnia 2023 14:05:46
```

## Deep Dive
W PowerShell, `Get-Date` to podstawowe polecenie do pobierania aktualnej daty i czasu systemowego. Istnieje od początków PowerShell i, co ciekawe, jest wrapperem do .NETowej klasy `DateTime`. Jeśli potrzebujesz większej kontroli, alternatywnie możesz użyć `[datetime]::Now` z .NET, zaś do zapisywania i porównywania dat przydatny może być obiekt `DateTimeOffset`.

Opcje formatowania, jak `-Format`, pozwalają dostosować wyświetlanie daty do potrzeb. Da się też z łatwością dodać lub odjąć określony czas, używając metod `AddDays()`, `AddMonths()` itp. na zwróconym obiekcie.

## See Also
- [Dokumentacja polecenia Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [Dokumentacja .NET DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [Formatowanie daty i czasu w PowerShell](https://ss64.com/ps/syntax-dateformats.html)
