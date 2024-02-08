---
title:                "Het gebruik van een interactieve shell (REPL)"
aliases:
- nl/c-sharp/using-an-interactive-shell-repl.md
date:                  2024-01-28T22:09:09.490295-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het gebruik van een interactieve shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c-sharp/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een REPL, of Read-Eval-Print Loop, stelt je in staat om C# code te typen en deze interactief uit te voeren. Programmeurs gebruiken het voor snelle experimenten, debuggen, of om C# te leren, zonder de overhead van het opzetten van volledige projecten.

## Hoe te:
Start een REPL in je C# omgeving met behulp van het C# Interactieve venster of voer `dotnet-script` uit in je terminal. Hier is een voorproefje van het gebruik ervan:

```csharp
> var greeting = "Hallo, REPL!";
> Console.WriteLine(greeting);
Hallo, REPL!
>
```

Je krijgt direct feedback. Geen compileer- en uitvoerstappen. Gewoon coderen en zien.

## Diepere Duik
REPL reisde van Lisp naar moderne talen, bloeiend in dynamische talen zoals Python. Met C#, bracht Roslyn de REPL dichter bij ontwikkelaars. `csi` voor Roslyn, en `dotnet-script` voor .NET Core, zijn solide opties. Een diepere snede: ze evalueren code per regel, niet alles tegelijk, een ander uitvoeringsmodel versus typische C# apps. Dit heeft invloed op de staatpersistentie tussen uitvoeringen en de reikwijdte van variabelen.

Het C# Interactieve venster van Visual Studio is een door Roslyn aangedreven REPL. Het heeft Intellisense, meerdere referenties en ondersteuning voor NuGet-pakketten. Een flinke stap vooruit ten opzichte van vroege commandolijnexperimenten.

Voor alternatieve talen gebruikt Python `IDLE`, JavaScript heeft Node.js's REPL, en F# wordt geleverd met `F# Interactive`. Elk bevordert directe feedbacklussen, onschatbaar voor het testen van kleine codefragmenten of het begrijpen van taalfuncties.

## Zie ook
- [.NET Core `dotnet-script` REPL](https://github.com/filipw/dotnet-script)
