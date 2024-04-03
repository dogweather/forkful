---
date: 2024-01-26 04:12:22.043848-07:00
description: "Hvordan: Start en REPL i ditt C#-milj\xF8 ved \xE5 bruke C# Interactive-vinduet\
  \ eller kj\xF8r `dotnet-script` i terminalen din. Her er en smakebit p\xE5 hvordan\
  \ det\u2026"
lastmod: '2024-03-13T22:44:40.797356-06:00'
model: gpt-4-0125-preview
summary: "Start en REPL i ditt C#-milj\xF8 ved \xE5 bruke C# Interactive-vinduet eller\
  \ kj\xF8r `dotnet-script` i terminalen din."
title: Bruke et interaktivt skall (REPL)
weight: 34
---

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
