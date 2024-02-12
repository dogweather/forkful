---
title:                "Korzystanie z interaktywnego shella (REPL)"
aliases:
- /pl/c-sharp/using-an-interactive-shell-repl/
date:                  2024-01-26T04:12:26.437455-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z interaktywnego shella (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
REPL, czyli pętla Czytaj-Wykonaj-Wypisz, pozwala wpisywać i interaktywnie uruchamiać kod w C#. Programiści używają go do szybkich eksperymentów, debugowania lub nauki C#, bez konieczności konfigurowania pełnych projektów.

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
