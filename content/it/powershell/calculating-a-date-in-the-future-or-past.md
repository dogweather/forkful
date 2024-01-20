---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "PowerShell: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Calcolare una data futura o passata significa determinare una specifica data che si verifica prima o dopo una data di riferimento. I programmatori lo fanno per pianificare eventi, impostare rimesse o semplicemente per svolgere calcoli temporali.

## Come si fa:

Ecco un esempio di come calcolare una data futura in PowerShell:

```PowerShell
$DataCorrente = Get-Date
$DataFutura = $DataCorrente.AddDays(30)
Write-Output $DataFutura
```

Questo calcola una data 30 giorni nel futuro rispetto alla data corrente. Ecco un esempio dell'output:

```PowerShell
Giovedì 4 novembre 2021 14:23:15
```

Per calcolare una data nel passato, utilizziamo una cifra negativa con `AddDays()`. Ecco un esempio:

```PowerShell
$DataCorrente = Get-Date
$DataPassata = $DataCorrente.AddDays(-30)
Write-Output $DataPassata
```
## Approfondimento

Oltre alla funzione `AddDays()`, PowerShell fornisce una serie di altre funzioni date che possono essere utilizzate per calcolare date future o passate, come `AddHours()`, `AddMinutes()`, `AddSeconds()`, `AddMilliseconds()`, `AddYears()` e `AddMonths()`. Questa è una caratteristica diversa da altri linguaggi di scripting come Bash, che non hanno tali funzioni integrate e richiedono librerie esterne per operazioni simili.

Un'alternativa all'utilizzo di `AddDays()` in PowerShell è utilizzare l'operatore di aggiunta date-time (`+`). Per esempio:

```PowerShell
$DataCorrente = Get-Date
$DataFutura = $DataCorrente + (New-TimeSpan -Days 30)
```

PowerShell calcola le date future e passate basandosi sull'ora UTC, piuttosto che sull'ora locale. Questo è importante da capire se si sta pianificando qualcosa che ha delle implicazioni di fuso orario.

## Vedi Anche

- [Documentazione ufficiale di Microsoft su Get-Date](https://docs.microsoft.com/it-it/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [Documentazione ufficiale di Microsoft su AddDays](https://docs.microsoft.com/it-it/dotnet/api/system.datetime.adddays?view=net-5.0)