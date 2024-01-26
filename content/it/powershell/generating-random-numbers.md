---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:49:32.949561-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generare numeri casuali è come tirare un dado virtuale: serve ad avere un valore imprevedibile. I programmatori lo fanno per test, sicurezza, giochi, e dove serve l'aleatorietà.

## How to:
Ecco alcuni esempi su come generare numeri casuali in PowerShell:

```PowerShell
# Genera un numero casuale tra 0 e 100
$randomNumber = Get-Random -Maximum 100
$randomNumber
```

Output:
```
42
```

```PowerShell
# Genera un numero casuale compreso tra 50 e 100
$randomNumber = Get-Random -Minimum 50 -Maximum 100
$randomNumber
```

Output:
```
73
```

```PowerShell
# Genera un valore casuale da un array
$colors = 'rosso', 'verde', 'blu', 'giallo'
$randomColor = Get-Random -InputObject $colors
$randomColor
```

Output:
```
verde
```

## Deep Dive
PowerShell usa il comando `Get-Random` per generare numeri casuali. Questo comando è basato sui generatori di numeri pseudo-casuali del .NET framework. Storicamente, i numeri pseudo-casuali sono calcolati tramite algoritmi che simuleranno casualità.

Ci sono alternative a `Get-Random`. Per esempio, [System.Random] in .NET o [rng-tools] su sistemi Linux. Però `Get-Random` è integrato in PowerShell ed è più che sufficiente per la maggior parte dei casi d'uso.

Quando si parla di dettagli implementativi, `Get-Random` può opzionalmente prendere un "seed" che inizializza il generatore numerico, permettendo di riprodurre la stessa sequenza di numeri casuali, utile per i test.

## See Also
- [Microsoft Docs: Get-Random](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random)
- [Mozilla Developer Network: Math.random() for comparison](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [.NET API Documentation: System.Random](https://docs.microsoft.com/en-us/dotnet/api/system.random)
