---
date: 2024-01-26 04:20:04.751331-07:00
description: 'Wie geht das: Zuerst installiere einen TOML-Parser wie `Tomlyn`. Benutze
  deinen Paketmanager.'
lastmod: '2024-03-13T22:44:53.912494-06:00'
model: gpt-4-0125-preview
summary: Zuerst installiere einen TOML-Parser wie `Tomlyn`.
title: Arbeiten mit TOML
weight: 39
---

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
