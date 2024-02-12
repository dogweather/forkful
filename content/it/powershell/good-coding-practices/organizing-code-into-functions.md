---
title:                "Organizzazione del codice in funzioni"
aliases:
- /it/powershell/organizing-code-into-functions.md
date:                  2024-01-26T01:11:12.105091-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizzazione del codice in funzioni"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Organizzare il codice in funzioni significa racchiudere blocchi di codice che svolgono specifiche attività e assegnare loro un nome. Si fa per rendere il codice riutilizzabile, leggibile e mantenibile. Invece di riscrivere lo stesso codice, si chiama una funzione. Si vuole risolvere problemi o fare aggiornamenti? Modificare la funzione senza dover cercare tra mucchi di script.

## Come fare:
Scriviamo una funzione per calcolare la somma di due numeri. Semplice, ma illustra il concetto.

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# Chiamata della funzione con 5 e 10
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "La somma è $sum"
```

Esempio di output:

```
La somma è 15
```

## Analisi Approfondita
Le funzioni in PowerShell, come nella maggior parte dei linguaggi, non sono una novità. Abbiamo iniziato a compartimentare il codice dai tempi di Fortran. Si tratta di 'non reinventare la ruota'. Alternative? Certo, script o cmdlet. Ma mancano della pulizia e della sensibilità al contesto delle funzioni all'interno degli script.

Implementazione? Le funzioni possono essere basilari come il nostro esempio o complesse con ambiti, input di pipeline e altro. Prendiamo le `Funzioni Avanzate`. Imitano i cmdlet con parametri che hanno attributi, come `[Parameter(Mandatory=$true)]`. Questo è un assaggio della flessibilità di PowerShell.

## Vedi Anche
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/it-it/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/it-it/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
