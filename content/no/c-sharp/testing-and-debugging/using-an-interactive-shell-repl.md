---
date: 2024-01-26 04:12:22.043848-07:00
description: "En REPL, eller Read-Eval-Print Loop, lar deg skrive C#-kode og kj\xF8\
  re den interaktivt. Programmerere bruker det for raske eksperimenter, feils\xF8\
  king, eller\u2026"
lastmod: 2024-02-19 22:05:00.063755
model: gpt-4-0125-preview
summary: "En REPL, eller Read-Eval-Print Loop, lar deg skrive C#-kode og kj\xF8re\
  \ den interaktivt. Programmerere bruker det for raske eksperimenter, feils\xF8king,\
  \ eller\u2026"
title: Bruke et interaktivt skall (REPL)
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
