---
title:                "Convertire una stringa in minuscolo"
html_title:           "PowerShell: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Con "convertire una stringa in minuscolo" si intende trasformare tutte le lettere presenti in una stringa in minuscolo. I programmatori spesso lo fanno per uniformare il formato delle stringhe e facilitarne la gestione.

## Come fare:
```PowerShell
$stringa = "QuestA è UNa STRINGA"
$stringa.ToLower()
```

Ecco l'output: `questa è una stringa`.

## Approfondimento:
Convertire una stringa in minuscolo è un compito comune nella programmazione, poiché spesso è necessario confrontare due stringhe senza riguardare il caso delle lettere. In passato, era necessario utilizzare funzioni complesse per realizzare questa operazione, ma grazie a PowerShell è diventato molto più semplice e veloce.

Per coloro che preferiscono utilizzare comandi più brevi, esistono anche alternative come `ToLowerInvariant()`, che garantisce la stessa formattazione su qualsiasi sistema operativo.

Per quanto riguarda l'implementazione, PowerShell utilizza il metodo `.ToLower()` per trasformare tutte le lettere in minuscolo. Le lettere accentate e simboli speciali vengono convertiti correttamente secondo le regole della lingua selezionata nel sistema operativo.

## Vedi anche:
- Documentazione ufficiale di PowerShell su [ToLower()](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netframework-4.8)
- Guida pratica su come [trasformare le stringhe in PowerShell](https://adamtheautomator.com/string-functions-powershell/)