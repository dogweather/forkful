---
title:                "Jobbe med TOML"
date:                  2024-01-26T04:20:25.375944-07:00
model:                 gpt-4-0125-preview
simple_title:         "Jobbe med TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/working-with-toml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
TOML er et akronym for Toms Opplagte, Minimale Språk, et konfigurasjonsfilformat som er lett å lese på grunn av sin klare semantikk. Programmerere bruker det for konfigfiler, forenkler datautveksling mellom systemer, og fordi det oppnår en balanse mellom menneskelig lesbarhet og maskinell parserbarhet.

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
