---
title:                "Lavorare con i numeri complessi"
aliases:
- /it/powershell/working-with-complex-numbers.md
date:                  2024-01-26T04:44:11.447709-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con i numeri complessi"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/powershell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
I numeri complessi, quelli con una parte reale e una parte immaginaria (come 3 + 4i), sono fondamentali in ambiti come l'ingegneria, la fisica e la scienza dei dati. I programmatori li usano per simulazioni, elaborazione di segnali e la risoluzione di tipi specifici di problemi matematici.

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
