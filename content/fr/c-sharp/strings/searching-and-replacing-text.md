---
date: 2024-01-20 17:57:28.251859-07:00
description: "Chercher et remplacer du texte, c'est modifier des bouts de cha\xEE\
  nes par d'autres. Les d\xE9veloppeurs l'utilisent pour corriger des donn\xE9es,\
  \ formatter ou\u2026"
lastmod: '2024-03-13T22:44:57.770928-06:00'
model: gpt-4-1106-preview
summary: "Chercher et remplacer du texte, c'est modifier des bouts de cha\xEEnes par\
  \ d'autres."
title: Recherche et remplacement de texte
weight: 10
---

## What & Why? (Quoi et Pourquoi?)
Chercher et remplacer du texte, c'est modifier des bouts de chaînes par d'autres. Les développeurs l'utilisent pour corriger des données, formatter ou manipuler des strings rapidement.

## How to: (Comment faire:)
Voici quelques exemples pour fouiller et substituer des textes en C# :

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string source = "Bonjour, le monde!";
        string search = "monde";
        string replace = "univers";

        // Simple remplacement
        string result = source.Replace(search, replace);
        Console.WriteLine(result); // Bonjour, l'univers!

        // Avec Regex pour plus de contrôle
        string pattern = "\\ble\\b";
        string regexResult = Regex.Replace(source, pattern, "la");
        Console.WriteLine(regexResult); // Bonjour, la monde!
    }
}
```
Sortie :
```
Bonjour, l'univers!
Bonjour, la monde!
```
## Deep Dive (Plongée en profondeur)
Historiquement, la recherche et le remplacement de texte ont leurs racines dans les éditeurs de texte des premiers jours de l'informatique. En C#, `String.Replace()` et les expressions régulières (Regex) sont les deux moyens principaux. `String.Replace()` est simple et rapide. Regex, lui, offre plus de souplesse grâce à la possibilité de motifs complexes. Concernant l'implémentation, `System.Text.RegularExpressions.Regex` est compilé en IL (Intermediate Language) et optimisé par le CLR (Common Language Runtime) pour le performance.

## See Also (Voir aussi)
Pour plus d'approfondissements :

- Documentation Microsoft sur `String.Replace()`: [Link](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace)
- Documentation Microsoft sur `Regex.Replace()`: [Link](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace)
- Tutoriel sur les expressions régulières en C#: [Link](https://www.regular-expressions.info/dotnet.html)
