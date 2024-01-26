---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:37:57.924896-07:00
html_title:           "Arduino: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Interpretare una data da una stringa significa trasformarla in un formato che il computer capisce. I programmatori lo fanno per manipolare o confrontare date in maniera automatica.

## How to:
In PowerShell, usi `[datetime]` o il metodo `ParseExact` per convertire stringhe in date. Ecco due esempi:

```PowerShell
# Uso semplice con casting
$dataStringa = "31/03/2023"
$data = [datetime]$dataStringa
$data
```

Output:
```
venerdì 31 marzo 2023 00:00:00
```

```PowerShell
# Uso avanzato con ParseExact
$dataStringa = "31-03-2023 14:00"
$formato = "dd-MM-yyyy HH:mm"
$culture = [System.Globalization.CultureInfo]::InvariantCulture
$data = [datetime]::ParseExact($dataStringa, $formato, $culture)
$data
```

Output:
```
venerdì 31 marzo 2023 14:00:00
```

## Deep Dive
PowerShell usa le classi del .NET Framework per lavorare con date e orari. Questo risale ai primi giorni del .NET, introducendo un modo standardizzato per gestire le date.

Ci sono alternative come `Get-Date` per il parsing più flessibile e la manipolazione delle date:

```PowerShell
# Parsing flessibile con Get-Date
$dataStringa = "2023-03-31"
$data = Get-Date $dataStringa
$data
```

I dettagli implementativi da tener presente includono la gestione del formato della data e dell'ora, fuso orario e culture diverse, che possono influenzare come interpretare la stringa.

## See Also
- [La classe DateTime in .NET](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [Informazioni sulla cultura in .NET](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-6.0)
