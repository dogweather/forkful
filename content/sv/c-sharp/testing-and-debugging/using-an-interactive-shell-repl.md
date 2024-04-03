---
date: 2024-01-26 04:12:25.633148-07:00
description: "En REPL, eller Read-Eval-Print Loop, l\xE5ter dig skriva C#-kod och\
  \ k\xF6ra den interaktivt. Programmerare anv\xE4nder det f\xF6r snabba experiment,\
  \ fels\xF6kning eller\u2026"
lastmod: '2024-03-13T22:44:37.914872-06:00'
model: gpt-4-0125-preview
summary: "En REPL, eller Read-Eval-Print Loop, l\xE5ter dig skriva C#-kod och k\xF6\
  ra den interaktivt."
title: "Anv\xE4nda en interaktiv skal (REPL)"
weight: 34
---

## Vad & Varför?
En REPL, eller Read-Eval-Print Loop, låter dig skriva C#-kod och köra den interaktivt. Programmerare använder det för snabba experiment, felsökning eller för att lära sig C#, utan den extra börda som det innebär att sätta upp fullständiga projekt.

## Hur man gör:
Starta en REPL i din C#-miljö genom att använda C# Interactive-fönstret eller köra `dotnet-script` i din terminal. Här är ett smakprov på att använda den:

```csharp
> var greeting = "Hej, REPL!";
> Console.WriteLine(greeting);
Hej, REPL!
> 
```

Du får omedelbar feedback. Inga kompilerings- och körsteg. Bara koda och se.

## Fördjupning
REPL har rest från Lisp till moderna språk, och har frodats i dynamiska sådana som Python. Med C#, har Roslyn fört REPL närmare utvecklarna. `csi` för Roslyn och `dotnet-script` för .NET Core är solida alternativ. En djupare inblick: de utvärderar kod rad för rad, inte allt på en gång, en annan körningsmodell jämfört med typiska C#-applikationer. Detta påverkar beständigheten av tillstånd över körningar och variablers räckvidd.

Visual Studios C# Interactive-fönster är en REPL som drivs av Roslyn. Den har Intellisense, flera referenser och stöd för NuGet-paket. Ett rejält kliv upp från tidiga kommandoradsexperiment.

För alternativa språk använder Python `IDLE`, JavaScript har Node.js REPL och F# levereras med `F# Interactive`. Varje en bidrar till omedelbara feedbackloopar, ovärderliga för att testa små kodsnuttar eller förstå språkfunktioner.

## Se också
- [.NET Core `dotnet-script` REPL](https://github.com/filipw/dotnet-script)
