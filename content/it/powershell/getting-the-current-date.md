---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:16:09.106129-07:00
simple_title:         "Ottenere la data corrente"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Ottenere la data corrente è semplicemente recuperare l'informazione sulla data e ora attuali dal tuo sistema. I programmatori lo fanno per log, misurazioni di tempo e funzionalità dipendenti dalla data.

## How to:
Ottenere la data corrente in PowerShell è chiaro e semplice. Usa `Get-Date`:

```PowerShell
# Ottieni la data e l'ora correnti
$dataCorrente = Get-Date
echo $dataCorrente
```

Output tipico:
```
lunedì 2 gennaio 2023 15:25:35
```

Per formattare la data, aggiungi argomenti:

```PowerShell
# Data in formato specifico
echo (Get-Date -Format "yyyy-MM-dd")
```

Output tipico:
```
2023-01-02
```

## Deep Dive
`Get-Date` è un cmdlet PowerShell che esiste fin dalla prima versione, rilasciata nel 2006. È stato costruito per essere versatile e si è evoluto nel tempo.

Alternative:
Anche se `Get-Date` è lo standard, puoi anche usare il .NET class `[DateTime]`:

```PowerShell
[System.DateTime]::Now
```

Dettagli implementativi:
`Get-Date` offre un sacco di opzioni. Ti permette di cambiare la formattazione, aggiungere o sottrarre tempo e persino creare oggetti date personalizzati per qualsiasi timestamp.

## See Also
- [Get-Date Command | Microsoft Docs](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7)
- [Custom Date and Time Format Strings | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [DateTime Struct | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netframework-4.8)
