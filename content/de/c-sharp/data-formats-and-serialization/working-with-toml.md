---
title:                "Arbeiten mit TOML"
aliases:
- /de/c-sharp/working-with-toml.md
date:                  2024-01-26T04:20:04.751331-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/working-with-toml.md"
---

{{< edit_this_page >}}

## Was & Warum?
TOML ist ein Akronym für Toms Offensichtliche, Minimale Sprache, ein Konfigurationsdateiformat, das aufgrund seiner klaren Semantik leicht zu lesen ist. Programmierer verwenden es für Konfigurationsdateien, um den Datenaustausch zwischen Systemen zu vereinfachen, und weil es eine Balance zwischen menschlicher Lesbarkeit und maschineller Parsbarkeit findet.

## Wie geht das:
Zuerst installiere einen TOML-Parser wie `Tomlyn`. Benutze deinen Paketmanager:

```csharp
dotnet add package Tomlyn
```

Als Nächstes, parse eine TOML-Datei:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"Eigentümer: {tomlTable["owner"]["name"]}");
// Ausgabe:
// Eigentümer: Tom Preston-Werner
```

Nun erstelle und schreibe TOML:

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
Console.WriteLine("TOML wurde in config.toml geschrieben");
// Ausgabe:
// TOML wurde in config.toml geschrieben
```

## Tiefergehend:
TOML wurde etwa 2013 von Tom Preston-Werner, dem Mitbegründer von GitHub, als Reaktion auf die Beschränkungen bestehender Formate wie YAML und JSON in Konfigurationseinstellungen erstellt. Es ist speziell für Konfigurationen entworfen, mit einem starken Schwerpunkt auf Einfachheit und Eindeutigkeit.

Alternative Konfigurationsformate umfassen YAML, JSON und XML. Dennoch sticht TOML hervor, da es menschenfreundlicher ist, besonders bei Konfigurationsdateien, wo manuelles Bearbeiten üblich ist. JSON, obwohl allgegenwärtig, ist weniger lesbar für komplexe Konfigurationen, und XML ist ausschweifend. YAML, obwohl ähnlich in der Lesbarkeit, kann mit intensiver Verwendung von Leerzeichen kompliziert werden und birgt Sicherheitsrisiken bei bestimmten Inhalten.

In der Implementierung legt TOML den Fokus auf eine saubere Abbildung auf eine Hashtabelle, was die Datenextraktion vorhersagbar macht. Mit der Veröffentlichung der Version 1.0.0 hat TOML seine Spezifikation gefestigt, was die Stabilität und die Unterstützung von Werkzeugen verbessert.

## Siehe auch:
- Offizielles TOML GitHub-Repo & Spezifikation: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, die .NET-Bibliothek: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
