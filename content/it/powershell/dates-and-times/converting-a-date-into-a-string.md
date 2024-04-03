---
date: 2024-01-20 17:37:16.011577-07:00
description: "Convertire una data in una stringa permette di formattare il dato temporale\
  \ per una presentazione leggibile. Gli sviluppatori lo fanno spesso quando\u2026"
lastmod: '2024-03-13T22:44:43.654176-06:00'
model: gpt-4-1106-preview
summary: Convertire una data in una stringa permette di formattare il dato temporale
  per una presentazione leggibile.
title: Conversione di una data in una stringa
weight: 28
---

## How to:
Usa `Get-Date` e `ToString` per formattare date. Ecco alcuni esempi:

```PowerShell
# Ottenere la data corrente
$dataOggi = Get-Date

# Convertirla in stringa con formato predefinito
$dataInStringa = $dataOggi.ToString()
Write-Host "Data in formato predefinito: $dataInStringa"

# Convertire con formato personalizzato
$dataFormatPersonalizzato = $dataOggi.ToString('dd-MM-yyyy')
Write-Host "Data in formato personalizzato: $dataFormatPersonalizzato"
```

Output:
```
Data in formato predefinito: 03/31/2023 10:00:00 AM
Data in formato personalizzato: 31-03-2023
```

## Deep Dive
La conversione di date in stringhe esiste da molto tempo, essenziale per la condivisione dei dati tra sistemi e la presentazione all'utente. PowerShell consente di personalizzare questo formato in modi infiniti utilizzando `ToString` con specificatori di formato. La cultura del sistema può influenzare il formato predefinito, perciò specifica sempre il formato desiderato per evitare ambiguità.

Alternative includono l'uso di `Get-Date -Format`, che è più diretto per la formattazione veloce.

```PowerShell
# Formato rapido con Get-Date
$dataFormatRapido = Get-Date -Format 'yyyy/MM/dd'
Write-Host "Data con formato rapido: $dataFormatRapido"
```

A livello di implementazione, la conversione è basata sulle classi .NET DateTime e CultureInfo, che gestiscono le complessità delle differenze regionali e dei calendari.

## See Also
- [Custom Date and Time Format Strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [PowerShell Get-Date documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [PowerShell About Automatic Variables - $(Get-Culture)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.1#culture)
