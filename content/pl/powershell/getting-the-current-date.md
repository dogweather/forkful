---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Pobieranie bieżącej daty to proste zadanie, które polega na jednym z najbardziej podstawowych aspektów programowania. Kiedy programiści to robią? Zawsze, kiedy potrzebują założyć kontrolę nad czasem, na przykład do znaczników czasu, do wywołań API, do logów i jeszcze więcej.

## Jak to zrobić:
W PowerShell (obecnej wersji) pobieranie bieżącej daty jest tak proste, jak wpisanie jednej komendy.

```PowerShell
Get-Date
```

Podczas uruchomienia, wydrukuje bieżącą datę i czas, na przykład:

```PowerShell
środa, 20 października 2021 14:04:15
```

## Deep Dive
Powyższa komenda, Get-Date, można znaleźć w wersji PowerShell 2.0 i jest integralną częścią narzędzi od momentu jej wprowadzenia. Nie jest to jedyny sposób na pobranie bieżącej daty w PowerShell. Innymi opcjami są [System.DateTime]::Now i [System.DateTime]::Today, które zwracają pełny zestaw informacji czasowych i samą datę bez czasu odpowiednio.

Szczegóły implementacji dotyczące Get-Date wystarczają, aby dostarczyć aktualną datę i czas, jednak czasem możesz potrzebować bardziej szczegółowych informacji lub specyficznych elementów, takich jak miesiąc, dzień tygodnia itp. Dla tego można wykorzystać inne metody jak 'Get-Date -Format'.


## Zobacz też:
Emocjonujące jest, że istnieje wiele źródeł do nauki i doskonalenia umiejętności codingu w PowerShell!

- [Get-Date - Dokumentacja Microsoft](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [Formatowanie dat i czasu w PowerShell](http://www.computerperformance.co.uk/powershell/plus_get-date.htm)
- [System.DateTime Struktura](https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime?view=net-5.0)