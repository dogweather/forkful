---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:50.247459-07:00
description: "TOML is een acroniem voor Tom's Obvious, Minimal Language, een configuratiebestandsformaat\
  \ dat gemakkelijk te lezen is vanwege de duidelijke semantiek.\u2026"
lastmod: '2024-03-13T22:44:50.833500-06:00'
model: gpt-4-0125-preview
summary: "TOML is een acroniem voor Tom's Obvious, Minimal Language, een configuratiebestandsformaat\
  \ dat gemakkelijk te lezen is vanwege de duidelijke semantiek.\u2026"
title: Werken met TOML
---

{{< edit_this_page >}}

## Wat & Waarom?
TOML is een acroniem voor Tom's Obvious, Minimal Language, een configuratiebestandsformaat dat gemakkelijk te lezen is vanwege de duidelijke semantiek. Programmeurs gebruiken het voor configuratiebestanden, om gegevensuitwisseling tussen systemen te vereenvoudigen, en omdat het een evenwicht biedt tussen leesbaarheid voor mensen en parseerbaarheid door machines.

## Hoe:
Installeer eerst een TOML-parser zoals `Tomlyn`. Gebruik je pakketbeheerder:

```csharp
dotnet add package Tomlyn
```

Parseer vervolgens een TOML-bestand:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"Eigenaar: {tomlTable["owner"]["name"]}");
// Output:
// Eigenaar: Tom Preston-Werner
```

Maak nu TOML aan en schrijf het weg:

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
Console.WriteLine("TOML weggeschreven naar config.toml");
// Output:
// TOML weggeschreven naar config.toml
```

## Diepere duik:
TOML is gecreëerd door Tom Preston-Werner, de mede-oprichter van GitHub, rond 2013 als reactie op de beperkingen van bestaande formaten zoals YAML en JSON in configuratie-instellingen. Het is specifiek ontworpen voor configuraties met een sterke nadruk op eenvoud en ondubbelzinnigheid.

Alternatieve configuratieformaten omvatten YAML, JSON en XML. Toch valt TOML op vanwege de hogere gebruiksvriendelijkheid, in het bijzonder voor configuratiebestanden waarin vaak met de hand wordt bewerkt. JSON, hoewel alomtegenwoordig, is minder leesbaar voor complexe configuraties, en XML is omslachtig. YAML is qua leesbaarheid vergelijkbaar, maar kan ingewikkeld worden met intensief gebruik van witruimte en kent beveiligingsrisico's bij bepaalde inhoud.

Wat implementatie betreft, ligt de focus van TOML op een schone afbeelding naar een hash-tabel, waardoor gegevensextractie voorspelbaar wordt. Met de release van versie 1.0.0 heeft TOML zijn specificatie verstevigd, wat de stabiliteit en ondersteuning van tools verbeterde.

## Zie ook:
- Officiële TOML GitHub-repo & Spec: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, de .NET-bibliotheek: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
