---
date: 2024-01-20 17:38:00.054838-07:00
description: "How to: (Comment faire :) En C#, convertir une cha\xEEne en minuscules\
  \ est simple avec la m\xE9thode `ToLower()`."
lastmod: '2024-04-05T21:53:59.259641-06:00'
model: gpt-4-1106-preview
summary: "(Comment faire :) En C#, convertir une cha\xEEne en minuscules est simple\
  \ avec la m\xE9thode `ToLower()`."
title: "Conversion d'une cha\xEEne de caract\xE8res en minuscules"
weight: 4
---

## How to: (Comment faire :)
En C#, convertir une chaîne en minuscules est simple avec la méthode `ToLower()`:

```csharp
string original = "Bonjour, Monde!";
string enMinuscules = original.ToLower();

Console.WriteLine(enMinuscules); // sortie: "bonjour, monde!"
```

## Deep Dive (Plongée en Profondeur)
Historiquement, manipuler la casse des caractères est essentielle pour la recherche et le tri de textes. C# utilise Unicode pour gérer divers jeux de caractères, et `ToLower()` est culturellement sensible : il peut comporter différemment selon la culture spécifiée. 

Alternativement, `ToLowerInvariant()` ignore la culture locale et utilise les règles de la culture invariante, ce qui est utile pour les données techniques sans contexte de langue spécifique.

Les algorithmes de transformation tiennent compte des spécificités linguistiques, comme les caractères accentués ou la ligature. Ils sont optimisés pour limiter la charge sur la mémoire et le temps de traitement.

## See Also (Voir Aussi)
- Documentation Microsoft sur `ToLower()`: [Microsoft Docs: ToLower](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- Documentation Microsoft sur `ToLowerInvariant()`: [Microsoft Docs: ToLowerInvariant](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant)
- Comparaison de chaînes dans C#: [Microsoft Docs: StringComparison](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/compare-strings)
