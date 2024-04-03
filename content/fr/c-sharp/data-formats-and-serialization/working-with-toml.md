---
date: 2024-01-26 04:20:04.688360-07:00
description: "TOML est un acronyme pour Tom's Obvious, Minimal Language, un format\
  \ de fichier de configuration qui est facile \xE0 lire gr\xE2ce \xE0 sa s\xE9mantique\
  \ claire. Les\u2026"
lastmod: '2024-03-13T22:44:57.813682-06:00'
model: gpt-4-0125-preview
summary: "TOML est un acronyme pour Tom's Obvious, Minimal Language, un format de\
  \ fichier de configuration qui est facile \xE0 lire gr\xE2ce \xE0 sa s\xE9mantique\
  \ claire."
title: Travailler avec TOML
weight: 39
---

## Comment faire :
D'abord, installez un analyseur TOML comme `Tomlyn`. Utilisez votre gestionnaire de paquets :

```csharp
dotnet add package Tomlyn
```

Ensuite, analysez un fichier TOML :

```csharp
using Tomlyn;
using Tomlyn.Model;
using System;

var tomlContent = @"
[owner]
name = 'Tom Preston-Werner'
dob = 1979-05-27T07:32:00Z";

var tomlTable = Toml.Parse(tomlContent).ToModel();

Console.WriteLine($"Propriétaire : {tomlTable["owner"]["name"]}");
// Sortie :
// Propriétaire : Tom Preston-Werner
```

Maintenant, créez et écrivez du TOML :

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
Console.WriteLine("TOML écrit dans config.toml");
// Sortie :
// TOML écrit dans config.toml
```

## Approfondissement :
TOML a été créé par Tom Preston-Werner, le co-fondateur de GitHub, vers 2013 en réaction aux limitations des formats existants comme YAML et JSON dans les paramètres de configuration. Il est spécifiquement conçu pour les configurations avec un fort accent sur le fait d'être simple et sans ambiguïté.

Les formats de configuration alternatifs incluent YAML, JSON et XML. Pourtant, TOML se distingue par sa plus grande convivialité pour l'humain, en particulier pour les fichiers de configuration où l'édition à la main est courante. JSON, bien qu'omniprésent, est moins lisible pour les configurations complexes, et XML est verbeux. YAML, bien qu'équivalent en termes de lisibilité, peut se compliquer en raison de l'utilisation intensive des espaces blancs et comporte des risques de sécurité avec certains contenus.

En termes d'implémentation, TOML se concentre sur une correspondance claire avec une table de hachage, rendant l'extraction des données prévisible. Avec la sortie de la version 1.0.0, TOML a consolidé sa spécification, améliorant la stabilité et le support des outils.

## Voir aussi :
- Le repo GitHub officiel de TOML & Spec : [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Tomlyn, la bibliothèque .NET : [github.com/xoofx/Tomlyn](https://github.com/xoofx/Tomlyn)
