---
title:                "Bruke et interaktivt skall (REPL)"
date:                  2024-01-26T04:12:22.043848-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke et interaktivt skall (REPL)"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En REPL, eller Read-Eval-Print Loop, lar deg skrive C#-kode og kjøre den interaktivt. Programmerere bruker det for raske eksperimenter, feilsøking, eller læring av C#, uten overheadet av å sette opp hele prosjekter.

## Hvordan:
Start en REPL i ditt C#-miljø ved å bruke C# Interactive-vinduet eller kjør `dotnet-script` i terminalen din. Her er en smakebit på hvordan det brukes:

```csharp
> var greeting = "Hei, REPL!";
> Console.WriteLine(greeting);
Hei, REPL!
>
```

Du får umiddelbar tilbakemelding. Ingen kompilerings- og kjøringssteg. Bare kod og se.

## Dypdykk
REPL reiste fra Lisp til moderne språk, trivdes i dynamiske språk som Python. Med C#, brakte Roslyn REPL nærmere utviklerne. `csi` for Roslyn, og `dotnet-script` for .NET Core, er solide alternativer. Et dypere kutt: de evaluerer kode per linje, ikke alt sammen, en annen utførelsesmodell versus typiske C#-applikasjoner. Dette påvirker tilstandenes varighet over utførelser og variablenes omfang.

Visual Studios C# Interactive-vindu er en REPL drevet av Roslyn. Den har Intellisense, flere referanser, og støtte for NuGet-pakker. Ganske et steg opp fra tidlige kommandolinjeeksperimenter.

For alternative språk, bruker Python `IDLE`, JavaScript har Node.js sin REPL, og F# leveres med `F# Interactive`. Hver fremmer umiddelbare tilbakemeldingsløkker, uvurderlige for testing av små kodeutdrag eller forståelse av språkegenskaper.

## Se Også
- [.NET Core `dotnet-script` REPL](https://github.com/filipw/dotnet-script)
