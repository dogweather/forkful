---
date: 2024-01-26 04:44:11.447709-07:00
description: "Come fare: PowerShell non ha un supporto incorporato per i numeri complessi,\
  \ quindi puoi o creare una tua soluzione o utilizzare `System.Numerics.Complex`\u2026"
lastmod: '2024-03-13T22:44:43.635204-06:00'
model: gpt-4-0125-preview
summary: PowerShell non ha un supporto incorporato per i numeri complessi, quindi
  puoi o creare una tua soluzione o utilizzare `System.Numerics.Complex` di .NET.
title: Lavorare con i numeri complessi
weight: 14
---

## Come fare:
PowerShell non ha un supporto incorporato per i numeri complessi, quindi puoi o creare una tua soluzione o utilizzare `System.Numerics.Complex` di .NET.

```PowerShell
# Creiamo numeri complessi usando .NET
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# Creare numeri complessi
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# Sommare due numeri complessi
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# Moltiplicare due numeri complessi
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# Visualizzare i risultati
"Somma: $sum"
"Prodotto: $product"
```
Output:
```
Somma: (4, 6)
Prodotto: (-5, 10)
```

## Approfondimento
I numeri complessi sono stati sviluppati nel XVI secolo per risolvere equazioni che non avevano soluzioni nel regno dei numeri reali. Ora sono una pietra miliare della matematica moderna.

La dipendenza di PowerShell da .NET per il supporto dei numeri complessi significa che la performance è solida. Le alternative includono librerie di terze parti o altri linguaggi di programmazione come Python, dove i numeri complessi sono un tipo di dato nativo.

## Vedi Anche
- [Struttura System.Numerics.Complex](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [Aritmetica dei Numeri Complessi in Python](https://docs.python.org/3/library/cmath.html)
