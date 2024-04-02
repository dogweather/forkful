---
date: 2024-01-20 17:45:13.701015-07:00
description: "Extraire des sous-cha\xEEnes, c'est d\xE9couper une partie d'une cha\xEE\
  ne de caract\xE8res. On fait \xE7a pour analyser, manipuler ou valider des morceaux\
  \ sp\xE9cifiques\u2026"
lastmod: '2024-03-13T22:44:57.774944-06:00'
model: gpt-4-1106-preview
summary: "Extraire des sous-cha\xEEnes, c'est d\xE9couper une partie d'une cha\xEE\
  ne de caract\xE8res. On fait \xE7a pour analyser, manipuler ou valider des morceaux\
  \ sp\xE9cifiques\u2026"
title: "Extraction de sous-cha\xEEnes"
weight: 6
---

## Quoi et Pourquoi ?
Extraire des sous-chaînes, c'est découper une partie d'une chaîne de caractères. On fait ça pour analyser, manipuler ou valider des morceaux spécifiques d'une donnée texte.

## Comment faire :
Voici un peu de code C# pour montrer comment extraire des substrings. C'est simple, mate ça :

```C#
using System;

class Program
{
    static void Main()
    {
        string phrase = "La vie est belle";
        string sousChaine = phrase.Substring(3, 4); // on prend 4 caractères à partir de l'index 3
        Console.WriteLine(sousChaine);  // Affiche "vie "
    }
}
```
Résultat:
```
vie 
```

## Plongée Profonde
Historiquement, `.Substring()` est là depuis les premiers jours du .NET. C'est direct, mais manque de flexibilité. Pour être plus souple, on peut regarder `Span<T>` ou `ReadOnlySpan<T>` qui sont apparus dans C# 7.2. Ils permettent de faire des trucs sur des sous-sections sans créer de nouvelles chaînes. Plus efficace, surtout avec de gros textes.

## Voir Aussi
Pour aller plus loin, voici quelques ressources à explorer :

- Microsoft Documentation on String.Substring: [String.Substring Method - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
