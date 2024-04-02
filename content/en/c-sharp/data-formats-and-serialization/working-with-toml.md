---
date: 2024-01-25 03:39:57.074924-07:00
description: "TOML is an acronym for Tom's Obvious, Minimal Language, a configuration\
  \ file format that's easy to read due to its clear semantics. Programmers use it\
  \ for\u2026"
lastmod: '2024-03-13T22:45:00.110621-06:00'
model: gpt-4-1106-preview
summary: "TOML is an acronym for Tom's Obvious, Minimal Language, a configuration\
  \ file format that's easy to read due to its clear semantics. Programmers use it\
  \ for\u2026"
title: Working with TOML
weight: 39
---

## What & Why?
TOML is an acronym for Tom's Obvious, Minimal Language, a configuration file format that's easy to read due to its clear semantics. Programmers use it for config files, simplifying data interchange between systems, and because it strikes a balance between human readability and machine parseability.

## How to:
First, install a TOML parser like `Tomlyn`. Use your package manager:

```csharp
dotnet add package Tomlyn
```

Next, parse a TOML file:

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"Owner: {tomlTable["owner"]["name"]}");
// Output:
// Owner: Tom Preston-Werner
```

Now, create and write TOML:

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
Console.WriteLine("TOML written to config.toml");
// Output:
// TOML written to config.toml
```

## Deep Dive:
TOML was created by Tom Preston-Werner, GitHub's co-founder, around 2013 as a reaction to the limitations of existing formats like YAML and JSON in configuration settings. It's specifically designed for configs with a strong emphasis on being straightforward and unambiguous.

Alternative config formats include YAML, JSON, and XML. Yet, TOML stands out for being more human-friendly, particularly for configuration files where editing by hand is common. JSON, while ubiquitous, is less readable for complex configs, and XML is verbose. YAML, though similar in readability, can get complicated with heavy use of whitespace and has security risks with certain content.

Implementation-wise, TOML focuses on mapping cleanly to a hash table, making data extraction predictable. With version 1.0.0 released, TOML solidified its spec, improving stability and tooling support.

## See Also:
- Official TOML GitHub Repo & Spec: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, the .NET library: [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
