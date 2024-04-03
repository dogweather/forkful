---
date: 2024-01-26 04:20:25.375944-07:00
description: "Hvordan: F\xF8rst, installer en TOML-parser som `Tomlyn`. Bruk pakkebehandleren\
  \ din."
lastmod: '2024-03-13T22:44:40.821681-06:00'
model: gpt-4-0125-preview
summary: "F\xF8rst, installer en TOML-parser som `Tomlyn`."
title: Jobbe med TOML
weight: 39
---

## Hvordan:
Først, installer en TOML-parser som `Tomlyn`. Bruk pakkebehandleren din:

```csharp
dotnet add package Tomlyn
```

Deretter, parse en TOML-fil:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlInnhold = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTabell = Toml.Parse(tomlInnhold).ToModel();

Console.WriteLine($"Eier: {tomlTabell["owner"]["name"]}");
// Utdata:
// Eier: Tom Preston-Werner
```

Nå, opprett og skriv TOML:

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

var tomlStreng = doc.ToString();
File.WriteAllText("config.toml", tomlStreng);
Console.WriteLine("TOML skrevet til config.toml");
// Utdata:
// TOML skrevet til config.toml
```

## Dykk Dypt:
TOML ble skapt av Tom Preston-Werner, medgründer av GitHub, rundt 2013 som en reaksjon på begrensningene til eksisterende formater som YAML og JSON i konfigurasjonsinnstillinger. Det er spesielt designet for konfiger med en sterk vektlegging på å være enkel og utvetydig.

Alternative konfigurasjonsformater inkluderer YAML, JSON og XML. Likevel, TOML utmerker seg for å være mer menneskevennlig, spesielt for konfigurasjonsfiler hvor håndredigering er vanlig. JSON, mens det er allestedsnærværende, er mindre leselig for komplekse konfiger, og XML er ordrikt. YAML, selv om det ligner i lesbarhet, kan bli komplisert med tung bruk av hvitt rom og har sikkerhetsrisikoer med visse innhold.

Når det gjelder implementasjon, fokuserer TOML på å kartlegge rent til en hashtabell, noe som gjør dataekstraksjon forutsigbar. Med versjon 1.0.0 utgitt, solidifiserte TOML sin spesifikasjon, forbedret stabilitet og verktøystøtte.

## Se Også:
- Offisiell TOML GitHub Repo & Spec: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, .NET-biblioteket: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
