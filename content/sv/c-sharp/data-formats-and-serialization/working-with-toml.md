---
date: 2024-01-26 04:20:23.253733-07:00
description: "TOML \xE4r en f\xF6rkortning f\xF6r Toms Obvious, Minimal Language,\
  \ ett konfigurationsfilsformat som \xE4r l\xE4tt att l\xE4sa p\xE5 grund av dess\
  \ tydliga semantik.\u2026"
lastmod: '2024-03-13T22:44:37.937079-06:00'
model: gpt-4-0125-preview
summary: "TOML \xE4r en f\xF6rkortning f\xF6r Toms Obvious, Minimal Language, ett\
  \ konfigurationsfilsformat som \xE4r l\xE4tt att l\xE4sa p\xE5 grund av dess tydliga\
  \ semantik.\u2026"
title: Att arbeta med TOML
weight: 39
---

## Vad & Varför?
TOML är en förkortning för Toms Obvious, Minimal Language, ett konfigurationsfilsformat som är lätt att läsa på grund av dess tydliga semantik. Programmerare använder det för konfigurationsfiler, för att förenkla datautbyte mellan system, och eftersom det uppnår en balans mellan mänsklig läsbarhet och maskinell tolkningsbarhet.

## Hur man gör:
Först, installera en TOML-tolk, som `Tomlyn`. Använd din pakethanterare:

```csharp
dotnet add package Tomlyn
```

Nästa, tolka en TOML-fil:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"Ägare: {tomlTable["owner"]["name"]}");
// Utskrift:
// Ägare: Tom Preston-Werner
```

Nu, skapa och skriv TOML:

```csharp
using Tomlyn;
using Tomlyn.Syntax;
using System;
using System.IO;

var doc = new DocumentSyntax
{
    Tables =
    {
        new TableSyntax("owner")
        {
            Items =
            {
                { "name", "Tom Preston-Werner" },
                { "dob", "1979-05-27T07:32:00Z" }
            }
        }
    }
};

var tomlString = doc.ToString();
File.WriteAllText("config.toml", tomlString);
Console.WriteLine("TOML skrivet till config.toml");
// Utskrift:
// TOML skrivet till config.toml
```

## Fördjupning:
TOML skapades av Tom Preston-Werner, medgrundare av GitHub, runt 2013 som en reaktion mot begränsningarna hos befintliga format som YAML och JSON i konfigurationsinställningar. Det är specifikt utformat för konfigurationer med en stark betoning på att vara rakt på sak och entydigt.

Alternativa konfigurationsformat inkluderar YAML, JSON och XML. Ändå sticker TOML ut för att vara mer människovänligt, särskilt för konfigurationsfiler där det är vanligt att redigera för hand. JSON, som är allestädes närvarande, är mindre läsbart för komplexa konfigurationer, och XML är utförligt. YAML, som är liknande i läsbarhet, kan bli komplicerat med tung användning av blanksteg och har säkerhetsrisker med viss innehåll.

Implementeringsmässigt fokuserar TOML på att mappa rent till en hashtabell, vilket gör dataextraktion förutsägbar. Med version 1.0.0 utgiven, befäste TOML sin specifikation, och förbättrade stabilitet och verktygsstöd.

## Se även:
- Officiellt TOML GitHub-repo & specifikation: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, .NET-biblioteket: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
