---
date: 2024-01-20 17:31:35.571590-07:00
description: 'Come fare: Ecco esempi per calcolare date, utilizzando PowerShell. Calcolare
  una data 10 giorni nel futuro a partire da oggi.'
lastmod: '2024-03-13T22:44:43.655918-06:00'
model: gpt-4-1106-preview
summary: Ecco esempi per calcolare date, utilizzando PowerShell.
title: Calcolo di una data futura o passata
weight: 26
---

## Come fare:
Ecco esempi per calcolare date, utilizzando PowerShell.

Calcolare una data 10 giorni nel futuro a partire da oggi:
```PowerShell
$oggi = Get-Date
$futuro = $oggi.AddDays(10)
$futuro
```

Output esempio:
```
Giovedì 20 Aprile 2023 18:42:17
```

Calcolare una data 30 giorni nel passato:
```PowerShell
$oggi = Get-Date
$passato = $oggi.AddDays(-30)
$passato
```

Output esempio:
```
Domenica 12 Marzo 2023 18:42:17
```

## Approfondimento:
La funzione `AddDays()` usata negli esempi sopra è uno dei metodi per manipolare date in PowerShell. È semplice e diretto: aggiungi o sottrai giorni da una data specifica.

In passato, prima di linguaggi moderni come PowerShell, calcolare date richiedeva funzioni più complesse e a volte manipolazione manuale dei giorni, richiedendo attenzione a bisestili e a mesi con lunghezze diverse.

Ci sono anche alternative a `AddDays()`, come `AddHours()`, `AddMonths()`, e così via, per più granularità o intervalli temporali più ampi.

In teoria, potresti costruire una data da zero con `Set-Date`, ma è meno pratico per semplici calcoli futuri o passati.

## Vedi Anche:
- [Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [Set-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/set-date?view=powershell-7.1)
