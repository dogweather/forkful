---
title:                "Arrotondamento dei numeri"
date:                  2024-01-26T03:43:43.425978-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrotondamento dei numeri"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/rounding-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Arrotondare i numeri significa adeguarli al valore posizionale specificato più vicino—pensateli come semplificati. I programmatori arrotondano per controllare la precisione, migliorare le prestazioni o quando mostrano risultati amichevoli all'utente—come i prezzi che non necessitano di tre cifre decimali.

## Come fare:
Ecco il biglietto di andata e ritorno per arrotondare i numeri in C#:

```csharp
using System;

public class EsempiDiArrotondamento
{
    public static void Main()
    {
        double numeroOriginale = 123.4567;

        // Arrotonda al numero intero più vicino
        double arrotondato = Math.Round(numeroOriginale);
        Console.WriteLine(arrotondato); // Output: 123

        // Specifica il numero di cifre decimali
        double arrotondatoDueCifreDecimali = Math.Round(numeroOriginale, 2);
        Console.WriteLine(arrotondatoDueCifreDecimali); // Output: 123.46

        // Arrotonda sempre per eccesso indipendentemente dalla cifra successiva
        double arrotondatoPerEccesso = Math.Ceiling(numeroOriginale);
        Console.WriteLine(arrotondatoPerEccesso); // Output: 124

        // Arrotonda sempre per difetto indipendentemente dalla cifra successiva
        double arrotondatoPerDifetto = Math.Floor(numeroOriginale);
        Console.WriteLine(arrotondatoPerDifetto); // Output: 123
    }
}
```

## Approfondimento
Un tempo, arrotondare era uno stratagemma ovvio per ridurre i costi computazionali. Ogni ciclo contava, e tagliare i numeri risparmiava tempo prezioso. Avanzando fino al C# moderno, si tratta di gestire la famosa predisposizione di double e decimali agli errori di precisione e alle stranezze di visualizzazione.

Oltre a `Math.Round`, `Math.Floor` e `Math.Ceiling`, l'enumerazione `MidpointRounding` ci permette di decidere il destino di povere cifre in posizione mediana—è l'incrocio tra le regole bancarie e l'equità del parco giochi del "arrotonda per eccesso la metà".

Per pubblici più esigenti, come applicazioni serie di matematica o finanza, abbiamo `decimal` al posto di `double`, riducendo il dramma dell'arrotondamento offrendo una precisione maggiore—meno arrotondamenti, meno problemi.

## Vedi Anche
- [Documentazione Ufficiale di C# su `Math.Round`](https://docs.microsoft.com/it-it/dotnet/api/system.math.round)
- [Stack Overflow: Quando dovrei usare Double invece di Decimal?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [Standard IEEE per l’Aritmetica a Virgola Mobile (IEEE 754)](https://it.wikipedia.org/wiki/IEEE_754)
