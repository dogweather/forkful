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

Cosa e Perché?

Calcolare una data nel futuro o nel passato è una delle attività comuni per i programmatori. Ciò significa che è possibile modificare la data di un evento in modo che sia più lontana o più vicina rispetto all'attuale data. Di solito i programmatori fanno questo per automatizzare alcune funzioni o per gestire alcune esecuzioni di script in momenti specifici.

Come fare:

```PowerShell 
# Calcola la data di due settimane nel futuro
$date = Get-Date
$date.AddWeeks(2)

# Output: Sabato, Luglio 10, 2021 9:41:15 AM
```

```PowerShell
# Calcola la data di tre mesi nel passato
$date = Get-Date
$date.AddMonths(-3)

# Output: Martedì, Marzo 10, 2021 9:41:50 AM
```

Deep Dive:

Calcolare una data nel futuro o nel passato è un concetto che è stato utilizzato fin dai primi tempi di programmazione. In passato, i programmatori dovevano scrivere codice personalizzato per calcolare date e tempi specifici. Tuttavia, con lo sviluppo dei linguaggi di programmazione, è diventato molto più semplice utilizzare funzioni integrate per ottenere i risultati desiderati.

Ci sono anche alternative al calcolo della data nel futuro o nel passato, come l'utilizzo di librerie o software di terze parti progettati appositamente per questo scopo.

Per quanto riguarda l'implementazione, i programmatori possono utilizzare le funzioni integrate nei linguaggi di programmazione o le librerie di terze parti per ottenere i risultati desiderati.

See Also:

Per maggiori informazioni sul calcolo delle date nel futuro o nel passato, puoi consultare i seguenti link: 

- [Documentazione di Microsoft su come calcolare date nel futuro o nel passato in PowerShell](https://docs.microsoft.com/it-it/powershell/scripting/samples/calculating-dates?view=powershell-7.1)
- [Blogpost di scripter.pc su tre modi per calcolare date in PowerShell](https://blog.scripter.pc.it/calcolare-delle-date-in-powershell/)