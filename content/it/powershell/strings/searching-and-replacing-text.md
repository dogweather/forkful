---
title:                "Ricerca e sostituzione del testo"
aliases: - /it/powershell/searching-and-replacing-text.md
date:                  2024-01-20T17:58:30.050587-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ricerca e sostituzione del testo"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
La ricerca e sostituzione di testo permette di localizzare stringhe specifiche in un documento e cambiarle con altre. Programmare queste operazioni automatizza la modifica di molti file, risparmiando tempo e riducendo errori.

## How to:
Esempio semplice per cercare e sostituire una parola in una stringa:

```PowerShell
$text = "Ciao Mondo! Programmiamo con PowerShell."
$newText = $text -replace 'Mondo', 'Tutti'
$newText
```

Output:
```
Ciao Tutti! Programmiamo con PowerShell.
```

Per sostituire più pattern utilizzando regex:

```PowerShell
$text = "Le mele costano $3 al chilo, mentre le arance $2."
$text -replace '\$(\d+)', '€$1'
```

Output:
```
Le mele costano €3 al chilo, mentre le arance €2.
```

## Deep Dive
Il concetto di ricerca e sostituzione ha radici nella lavorazione di testi, con antenati meccanici come la macchina da scrivere. In informatica, un'implementazione iniziale fu l'editor di testo ed, presente nei primi sistemi Unix.

Alternative in PowerShell includono l'utilizzo di cmdlet come `Select-String` per la ricerca e l'uso combinato di `Get-Content` e `Set-Content` per la sostituzione in file. PowerShell, essendo basato sul framework .NET, permette anche l'uso di classi .NET come `System.Text.RegularExpressions.Regex` per gestire ricerca e sostituzione avanzate.

Dettagli implementativi importanti in PowerShell:
- `-replace` usa regex di default, quindi è potente ma bisogna stare attenti a caratteri speciali.
- Le stringhe sostituite non vengono modificate in-place; si crea una nuova stringa.
- Utilizzare parentesi tonde per catturare gruppi in regex e `$1`, `$2`, ecc., per riferirsi a questi gruppi nella stringa sostitutiva.

## See Also
- Tutorial su regex in PowerShell: [RegularExpression in PowerShell](https://ss64.com/ps/syntax-regex.html)
- Approfondimento su `Select-String`: [Microsoft Docs - Select-String](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Select-String?view=powershell-7.1)
