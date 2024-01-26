---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:48:57.649374-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generare numeri casuali significa creare valori imprevedibili durante l’esecuzione di un programma. Lo facciamo per giochi, simulazioni o per aggiungere sicurezza, come nei salti crittografici (crypto salts).

## How to:
Utilizzare `Random` per numeri semplici:

```C#
Random rnd = new Random();
int numeroCasuale = rnd.Next(1, 100); // da 1 a 99
Console.WriteLine(numeroCasuale);
```

Output di esempio:
```
42
```

Generare un numero casuale `double`:

```C#
double numeroDouble = rnd.NextDouble(); // da 0.0 a 1.0
Console.WriteLine(numeroDouble);
```

Output di esempio:
```
0,736177
```

Usare `RNGCryptoServiceProvider` per necessità di sicurezza maggiori:

```C#
using System.Security.Cryptography;

byte[] numeriCasuali = new byte[10];
using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
{
    rng.GetBytes(numeriCasuali);
}

Console.WriteLine(string.Join(", ", numeriCasuali));
```

Output di esempio:
```
182, 75, 159, 43, 89, 5, 173, 248, 238, 105
```

## Deep Dive
Prima era comune usare funzioni come `rand()` in C con semi basati su orari. Ora, in .NET, usiamo la classe `Random` o `RNGCryptoServiceProvider` per una maggiore sicurezza.

La classe `Random` è buona per casi generali, ma non per la crittografia dove la previsione comporta rischi. `RNGCryptoServiceProvider` è più lento ma più sicuro: genera numeri indovinabili.

I numeri non sono veramente casuali – sono pseudocasuali, basati su algoritmi. Il vero casuale richiede hardware specifico che legga dati caotici dal mondo reale.

## See Also
- Documentazione Microsoft su [`Random`](https://docs.microsoft.com/it-it/dotnet/api/system.random?view=net-7.0)
- Documentazione Microsoft su [`RNGCryptoServiceProvider`](https://docs.microsoft.com/it-it/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-7.0)
- Una guida alla [crittografia in .NET](https://docs.microsoft.com/it-it/dotnet/standard/security/cryptography-model)
- Spiegazione sulla [randomicità e pseudo-casualità](https://en.wikipedia.org/wiki/Pseudorandomness)
