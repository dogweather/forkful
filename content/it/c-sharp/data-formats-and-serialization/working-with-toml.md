---
date: 2024-01-26 04:20:14.240203-07:00
description: "TOML \xE8 un acronimo per Tom's Obvious, Minimal Language, un formato\
  \ di file di configurazione che \xE8 facile da leggere grazie alla sua semantica\
  \ chiara. I\u2026"
lastmod: '2024-03-13T22:44:43.457016-06:00'
model: gpt-4-0125-preview
summary: "TOML \xE8 un acronimo per Tom's Obvious, Minimal Language, un formato di\
  \ file di configurazione che \xE8 facile da leggere grazie alla sua semantica chiara."
title: Lavorare con TOML
weight: 39
---

## Come fare:
Prima, installa un parser TOML come `Tomlyn`. Usa il tuo gestore di pacchetti:

```csharp
dotnet add package Tomlyn
```

Successivamente, analizza un file TOML:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"Proprietario: {tomlTable["owner"]["name"]}");
// Output:
// Proprietario: Tom Preston-Werner
```

Ora, crea e scrivi TOML:

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
Console.WriteLine("TOML scritto su config.toml");
// Output:
// TOML scritto su config.toml
```

## Approfondimento:
TOML è stato creato da Tom Preston-Werner, co-fondatore di GitHub, intorno al 2013 come reazione alle limitazioni dei formati esistenti come YAML e JSON nelle impostazioni di configurazione. È specificamente progettato per le configurazioni con un forte enfasi su essere diretto e non ambiguo.

Alternative di formati di configurazione includono YAML, JSON e XML. Tuttavia, TOML si distingue per essere più amichevole per gli umani, in particolare per i file di configurazione dove la modifica manuale è comune. JSON, sebbene onnipresente, è meno leggibile per configurazioni complesse, e XML è verboso. YAML, sebbene simile in leggibilità, può diventare complesso con l'uso intenso di spazi bianchi e presenta rischi di sicurezza con determinati contenuti.

Dal punto di vista dell'implementazione, TOML si concentra su una mappatura pulita a una tabella hash, rendendo l'estrazione dei dati prevedibile. Con il rilascio della versione 1.0.0, TOML ha consolidato la sua specifica, migliorando stabilità e supporto agli strumenti.

## Vedi anche:
- Repo GitHub ufficiale di TOML & Spec: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, la libreria .NET: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
