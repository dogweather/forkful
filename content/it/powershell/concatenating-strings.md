---
title:                "Unione di stringhe"
html_title:           "PowerShell: Unione di stringhe"
simple_title:         "Unione di stringhe"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

La concatenazione di stringhe è un'operazione fondamentale nella programmazione. Si tratta di un processo in cui due o più stringhe vengono unite insieme in una sola. I programmatori utilizzano la concatenazione di stringhe per creare frasi, espressioni o testo dinamico all'interno dei loro script.

## Come fare:

Ecco alcuni esempi di codice in PowerShell per dimostrare come concatenare le stringhe.

```
#Esempio 1: Unire due stringhe usando l'operatore +

$nome = "Maria"
$cognome = "Rossi"

$nomeCompleto = $nome + " " + $cognome
Write-Output $nomeCompleto

#Output: Maria Rossi
```

```
#Esempio 2: Unire più stringhe usando l'operatore + all'interno di una singola espressione

$paese = "Italia"
$citta = "Roma"
$indirizzo = "Via Nazionale"

$indirizzoCompleto = "$indirizzo, $citta, $paese"
Write-Output $indirizzoCompleto

#Output: Via Nazionale, Roma, Italia
```

```
#Esempio 3: Unire un array di stringhe usando il metodo .Join()

$listaNomi = @("Giovanni", "Luca", "Sara")
$elencoNomi = $listaNomi -join ", "
Write-Output $elencoNomi

#Output: Giovanni, Luca, Sara
```

## Approfondimento:

La concatenazione di stringhe è un'operazione che ha origini nella programmazione delle prime versioni dei linguaggi di programmazione. In PowerShell, oltre all'uso dell'operatore + e del metodo .Join(), è possibile utilizzare anche il comando [string]::Concat() per unire le stringhe. Un'alternativa alla concatenazione di stringhe è l'uso di formattazione delle stringhe, che consente di inserire valori dinamici all'interno di una frase predefinita.

## Vedi anche:

Per ulteriori informazioni sulla concatenazione di stringhe in PowerShell, puoi consultare la documentazione ufficiale di Microsoft su [Concatenare stringhe in PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_strings?view=powershell-7.1#concatenating-strings-in-powershell).