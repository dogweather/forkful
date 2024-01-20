---
title:                "Calcolo di una data nel futuro o nel passato"
html_title:           "C#: Calcolo di una data nel futuro o nel passato"
simple_title:         "Calcolo di una data nel futuro o nel passato"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?

Calcolare una data nel futuro o nel passato è l'atto di determinare una data esatta spostandosi avanti o indietro da una data specifica. Questo è comunemente fatto dai programmatori per gestire gli eventi temporali nei loro programmi.

## Come si fa:

Ecco una rapida dimostrazione su come calcolare una data in futuro o passato in C#:

```C#
DateTime dataOggi= DateTime.Now;
DateTime dataFutura= dataOggi.AddDays(10);
DateTime dataPassata= dataOggi.AddDays(-5);
Console.WriteLine("Data di Oggi: " + dataOggi);
Console.WriteLine("Data Futura: " + dataFutura);
Console.WriteLine("Data Passata: " + dataPassata);
```

Questo snippet di codice restituisce un output simile a quello seguente:

```C#
Data di Oggi: 25/03/2023 13:50:06
Data Futura: 04/04/2023 13:50:06
Data Passata: 20/03/2023 13:50:06
```

## Approfondimenti:

Historicamente, il calcolo delle date si basava su metodi complessi, ma C# ordina le date e i tempi utilizzando l'oggetto DateTime. 

Esistono alternative, come l'utilizzo di un'API esterna o di librerie personalizzate che risolvono problemi come le differenze di fuso orario, ma l'uso di DateTime per queste operazioni rimane semplice ed efficiente. 

I metodi usati sopra, AddDays(), restituiscono un nuovo oggetto DateTime dato che DateTime è immutabile, quindi non cambiano mai il valore dell'oggetto originale.

## Vedi Anche:

Per approfondimenti e maggiori informazioni sul calcolo delle date e le manipolazioni di date e orari in C#, si possono consultare le seguenti risorse:

- [DateTime Struct (Microsoft Docs)](https://docs.microsoft.com/it-it/dotnet/api/system.datetime?view=net-6.0)
- [Handle Date and Time in C# .NET (PluralSight)](https://www.pluralsight.com/guides/handle-date-time-in-csharp-dotnet)