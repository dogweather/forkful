---
aliases:
- /fr/c-sharp/interpolating-a-string/
date: 2024-01-20 17:50:28.295690-07:00
description: "(\"Quoi et Pourquoi ?\") Interpoler une cha\xEEne, c'est ins\xE9rer\
  \ des valeurs de variables ou d'expressions dans une cha\xEEne de caract\xE8res.\
  \ On fait \xE7a pour\u2026"
lastmod: 2024-02-18 23:09:08.812214
model: gpt-4-1106-preview
summary: "(\"Quoi et Pourquoi ?\") Interpoler une cha\xEEne, c'est ins\xE9rer des\
  \ valeurs de variables ou d'expressions dans une cha\xEEne de caract\xE8res. On\
  \ fait \xE7a pour\u2026"
title: "Interpolation de cha\xEEnes de caract\xE8res"
---

{{< edit_this_page >}}

## What & Why?
("Quoi et Pourquoi ?")

Interpoler une chaîne, c'est insérer des valeurs de variables ou d'expressions dans une chaîne de caractères. On fait ça pour construire des textes dynamiques, facilement, sans concaténation compliquée.

## How to:
("Comment faire :")

```C#
// Exemple d'interpolation de chaîne en C#
string prenom = "Claire";
string message = $"Bonjour, {prenom}! Comment ça va?";
Console.WriteLine(message);
// Sortie: Bonjour, Claire! Comment ça va?
```

```C#
// Interpolation avec des expressions
int heures = 18;
string salutation = $"Bon{(heures < 12 ? "jour" : "soir")}, le monde!";
Console.WriteLine(salutation);
// Sortie: Bonsoir, le monde!
```

## Deep Dive
("Plongée en Profondeur")

Historiquement, en C#, on concaténait des chaînes avec l'opérateur `+`. Puis, `String.Format` est arrivé, plus lisible mais encore verbeux. L'interpolation de chaîne, introduite en C# 6, a rendu ce processus nettement plus propre et moins sujet aux erreurs.

Autres méthodes ? Oui, `StringBuilder` pour les situations exigeant de meilleures performances avec de nombreuses opérations. Mais pour la clarté ? L'interpolation gagne.

Côté implémentation, une chaîne interpolée est transformée en une chaîne formatée à l'exécution. Utilisez `FormattableString` si vous devez accéder au format et aux arguments séparément.

## See Also
("Voir Aussi")

- Documentation Microsoft sur l'interpolation: [docs.microsoft.com](https://docs.microsoft.com/fr-fr/dotnet/csharp/language-reference/tokens/interpolated)
- Comparaison des performances, StringBuilder vs. Interpolation: [BenchmarkDotNet](https://benchmarkdotnet.org)
