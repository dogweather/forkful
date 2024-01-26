---
title:                "Calcolo di una data futura o passata"
date:                  2024-01-20T17:31:06.426096-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcolo di una data futura o passata"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Calcolare una data nel futuro o nel passato significa semplicemente determinare esattamente una data prima o dopo un certo punto temporale. I programmatori lo fanno per gestire scadenze, eventi pianificati, ricorrenze, e per manipolare dati temporali in generale.

## How to:
C# rende il calcolo delle date piuttosto diretto con `DateTime` e `TimeSpan`. Ecco come farlo:

```C#
using System;

public class DateCalculator
{
    static void Main(string[] args)
    {
        DateTime oggi = DateTime.Now;
        TimeSpan unaSettimana = new TimeSpan(7, 0, 0, 0);
        
        DateTime futuro = oggi.AddDays(7);
        DateTime passato = oggi.Subtract(unaSettimana);
        
        Console.WriteLine("Data Futura: " + futuro.ToShortDateString());
        Console.WriteLine("Data Passata: " + passato.ToShortDateString());
    }
}
```
Output:
```
Data Futura: 20/04/2023
Data Passata: 06/04/2023
```

## Deep Dive
C# ha introdotto `DateTime` fin dalle prime versioni per soddisfare la necessità di manipolare date e orari. Prima, i programmatori erano costretti a fare calcoli manuali sui timestamp, che era complicato e soggetto a errori. `DateTimeOffset` è un'alternativa che considera i fusi orari. Mentre `TimeSpan` rappresenta una durata di tempo.

Per implementazioni più avanzate, ci sono librerie come NodaTime. NodaTime fornisce una gestione più robusta di date e orari, specialmente per quanto riguarda i fusi orari e il calendario internazionale.

## See Also
- Microsoft Docs: DateTime Struct - https://docs.microsoft.com/it-it/dotnet/api/system.datetime?view=net-6.0
- Microsoft Docs: TimeSpan Struct - https://docs.microsoft.com/it-it/dotnet/api/system.timespan?view=net-6.0
- NodaTime Documentation - https://nodatime.org/3.0.x/userguide
