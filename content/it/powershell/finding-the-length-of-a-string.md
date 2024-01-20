---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Haskell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è & Perché?
Trovare la lunghezza di una stringa significa contare il numero di caratteri in essa. I programmatori lo fanno per manipolare stringhe, validare input e lavorare con dati testuali.

## Come Fare:
```PowerShell
$stringa = "Programmazione PowerShell"
$lunghezza = $stringa.Length
Write-Output "La lunghezza della stringa è: $lunghezza"
```
Questo codice restituirà:
```PowerShell
La lunghezza della stringa è: 26
```
La proprietà `.Length` di una stringa ti dà il numero esatto di caratteri in essa.

## Approfondimento
Nel contesto storico, i primi linguaggi di programmazione non avevano modi incorporati per trovare la lunghezza di una stringa. I programmatori dovrebbero scorrere ogni carattere per contarli.

In PowerShell, oltre alla proprietà `.Length`, esiste anche il cmdlet `Measure-Object` che può essere utilizzato per trovare la lunghezza della stringa come alternativa.

```PowerShell
$stringa = "Programmazione PowerShell"
$lunghezza = ($stringa | Measure-Object -Character).Characters
Write-Output "La lunghezza della stringa è: $lunghezza"
```

`Measure-Object` è più flessibile ma più verboso. La situazione specifica determinerà quale metodo è il migliore da utilizzare.

## Vedi Anche
Per ulteriori informazioni sulle stringhe in PowerShell, consulta i seguenti link:

- [Microsoft Docs: Measure-Object](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/measure-object?view=powershell-7.1)