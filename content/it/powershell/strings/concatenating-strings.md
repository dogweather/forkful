---
date: 2024-01-20 17:35:41.130191-07:00
description: "How to: (Come fare:) Nelle prime versioni di PowerShell, concatenare\
  \ stringhe era spesso fatto esclusivamente con l'operatore `+`. Tuttavia, questo\
  \ pu\xF2\u2026"
lastmod: '2024-04-05T22:50:57.437520-06:00'
model: gpt-4-1106-preview
summary: (Come fare:) Nelle prime versioni di PowerShell, concatenare stringhe era
  spesso fatto esclusivamente con l'operatore `+`.
title: Concatenazione di stringhe
weight: 3
---

## How to: (Come fare:)
```PowerShell
# Utilizzando l'operatore `+`
$stringaUno = "Ciao, "
$stringaDue = "mondo!"
$stringaUnita = $stringaUno + $stringaDue
$stringaUnita  # Output: Ciao, mondo!

# Metodo .NET `String.Concat()`
$stringaUnita = [String]::Concat($stringaUno, $stringaDue)
$stringaUnita  # Output: Ciao, mondo!

# Interpolazione di stringhe con "$()"
$nome = "Fabio"
$saluto = "Buongiorno"
$frase = "$saluto, $nome!"
$frase  # Output: Buongiorno, Fabio!

# Utilizzando -join operatore
$elencoStringhe = "Uno", "Due", "Tre"
$stringaUnita = $elencoStringhe -join ' '
$stringaUnita  # Output: Uno Due Tre
```

## Deep Dive (Approfondimento)
Nelle prime versioni di PowerShell, concatenare stringhe era spesso fatto esclusivamente con l'operatore `+`. Tuttavia, questo può essere inefficiente con lunghe liste o in loop grandi, perché ogni concatenazione crea una nuova stringa.

Un'alternativa è l'uso del metodo .NET `String.Concat()`, che è più efficiente specialmente quando si uniscono molte stringhe. 

Poi c'è l'interpolazione di stringhe, introdotta con PowerShell 3.0, utilizzando la notazione `"$()"`. Questo metodo è più leggibile e in genere è il modo preferito quando si lavora con variabili.

L'operatore `-join` è utile quando abbiamo una collezione di valori che vogliamo unire in una singola stringa, separandoli con un delimitatore definito.

Sopra l'efficienza e la leggibilità, ogni metodo può essere preferito a seconda del contesto e delle necessità specifiche.

## See Also (Vedi Anche)
- [About Join Operator - Microsoft Docs](https://docs.microsoft.com/it-it/powershell/module/microsoft.powershell.core/about/about_Join)
- [String.Concat Method - Microsoft Docs](https://docs.microsoft.com/it-it/dotnet/api/system.string.concat?view=netframework-4.8)
