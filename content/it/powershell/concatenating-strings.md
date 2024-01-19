---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Concatenare le stringhe, in pratica, è l'unione di due o più stringhe per formare una singola stringa. I programmatori lo fanno per manipolare e presentare i dati in modo più significativo o utile.

## Come si fa:
Powershell fornisce vari modi per concatenare stringhe. Qui ci sono alcuni esempi:

```PowerShell
# Utilizzando l'operatore `+`
$stringa1 = "Ciao "
$stringa2 = "Mondo!"
$stringa3 = $stringa1 + $stringa2
echo $stringa3 
```
Output: `Ciao Mondo!`

```PowerShell
# Utilizzando il metodo `.Concat`
$stringa1 = [string]::Concat("Ciao ", "Mondo!")
echo $stringa1
```
Output: `Ciao Mondo!`

```PowerShell
# Utilizzando la formattazione delle stringhe
$stringa1 = "Ciao {0}!"
echo ($stringa1 -f "Mondo")
```
Output: `Ciao Mondo!`

## Approfondimento
Concatenare le stringhe è una pratica da lungo tempo in programmazione. PowerShell, essendo un linguaggio basato su .NET, eredita molti dei suoi metodi di concatenazione delle stringhe dal framework .NET. 

Ci sono numerose alternative per concatenare le stringhe in PowerShell, come l'uso di operatori di assegnazione composti (+=), il metodo StringBuilder e l'uso di guillemet (o virgolette).

Quando si tratta di implementazione, implica semplicemente l'assegnazione di una nuova variabile con l'unione delle stringhe.

## Consulta anche
1. [String interpolation: New and Improved in PowerShell 6, 7](https://adamtheautomator.com/powershell-string-interpolation/)
2. [How to Use PowerShell String Concatenation](https://www.top-password.com/knowledge/powershell-string-concatenation.html)
3. [Manipulating Strings in PowerShell](https://ss64.com/ps/syntax-operators.html)
4. [Running powershell with arguments](https://docs.microsoft.com/it-it/powershell/scripting/learn/ps101/10-functions?view=powershell-7.1)