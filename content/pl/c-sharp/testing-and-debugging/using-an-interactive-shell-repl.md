---
date: 2024-01-26 04:12:26.437455-07:00
description: "Jak u\u017Cywa\u0107: Uruchom REPL w swoim \u015Brodowisku C# korzystaj\u0105\
  c z okna C# Interactive lub uruchom `dotnet-script` w swoim terminalu. Oto ma\u0142\
  a pr\xF3bka jego\u2026"
lastmod: '2024-03-13T22:44:35.410026-06:00'
model: gpt-4-0125-preview
summary: "Uruchom REPL w swoim \u015Brodowisku C# korzystaj\u0105c z okna C# Interactive\
  \ lub uruchom `dotnet-script` w swoim terminalu."
title: Korzystanie z interaktywnego shella (REPL)
weight: 34
---

## Jak używać:
Uruchom REPL w swoim środowisku C# korzystając z okna C# Interactive lub uruchom `dotnet-script` w swoim terminalu. Oto mała próbka jego używania:

```csharp
> var greeting = "Cześć, REPL!";
> Console.WriteLine(greeting);
Cześć, REPL!
>
```

Otrzymujesz natychmiastową informację zwrotną. Bez kroków kompilacji i uruchamiania. Tylko koduj i zobacz.

## Szczegółowe omówienie
REPL podróżowało od Lispa do nowoczesnych języków, świetnie radząc sobie w dynamicznych, takich jak Python. W C#, Roslyn przybliżył REPL do programistów. `csi` dla Roslyn i `dotnet-script` dla .NET Core to solidne opcje. Głębszy wgląd: oceniają kod linia po linii, a nie wszystko naraz, co stanowi inny model wykonania w porównaniu z typowymi aplikacjami w C#. Ma to wpływ na trwałość stanu między wykonaniami oraz zakres zmiennych.

Okno C# Interactive w Visual Studio to REPL napędzany przez Roslyn. Posiada Intellisense, obsługę wielu referencji oraz pakietów NuGet. To spory krok naprzód w porównaniu z wczesnymi eksperymentami w wierszu poleceń.

Dla alternatywnych języków, Python używa `IDLE`, JavaScript ma REPL w Node.js, a F# dostarcza „F# Interactive”. Każdy z nich wspiera natychmiastową pętlę informacji zwrotnej, nieocenioną przy testowaniu małych fragmentów kodu lub rozumieniu cech języka.

## Zobacz również
- [REPL `dotnet-script` dla .NET Core](https://github.com/filipw/dotnet-script)
