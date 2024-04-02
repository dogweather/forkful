---
date: 2024-01-20 17:46:56.809897-07:00
description: "La longueur d'une cha\xEEne, c'est combien de caract\xE8res elle contient.\
  \ On mesure \xE7a pour tout, des mots de passe aux tweets : c'est essentiel pour\
  \ valider,\u2026"
lastmod: '2024-03-13T22:44:57.777100-06:00'
model: gpt-4-1106-preview
summary: "La longueur d'une cha\xEEne, c'est combien de caract\xE8res elle contient.\
  \ On mesure \xE7a pour tout, des mots de passe aux tweets : c'est essentiel pour\
  \ valider,\u2026"
title: "Trouver la longueur d'une cha\xEEne de caract\xE8res"
weight: 7
---

## What & Why?
La longueur d'une chaîne, c'est combien de caractères elle contient. On mesure ça pour tout, des mots de passe aux tweets : c'est essentiel pour valider, stocker ou manipuler le texte.

## How to:
En C#, c'est simple. Utilisez la propriété `.Length` sur une chaîne pour obtenir sa longueur. Voici comment :

```C#
string salutation = "Bonjour";
int longueur = salutation.Length;
Console.WriteLine("La longueur de la chaîne est: " + longueur);
```

Sortie :

```
La longueur de la chaîne est: 7
```

## Deep Dive
Avant C#, on trouvait déjà la longueur des chaînes en C avec `strlen`, mais c'était moins sûr. En C#, la propriété `.Length` est rapide et sécurisée. Elle renvoie un `int` représentant le nombre de caractères `Char` dans la chaîne. Attention aux chaînes nulles : si vous appelez `.Length` sur `null`, vous aurez une `NullReferenceException`. Utilisez `?.Length` pour éviter ça avec sécurité :

```C#
string salutation = null;
int? longueur = salutation?.Length;
Console.WriteLine("La longueur de la chaîne est: " + (longueur.HasValue ? longueur.Value.ToString() : "null"));
```

Autres façons ? On pourrait penser à parcourir la chaîne avec une boucle, mais pourquoi réinventer la roue ? La propriété `.Length` est optimale et intégrée.

La norme Unicode pourrait être pertinente ici. `.Length` compte les unités de code UTF-16, pas les points de code ou les graphèmes. Donc pour des caractères composés ou spéciaux, `.Length` pourrait surpris.

## See Also
- Microsoft Docs sur les propriétés des chaînes en C# : https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/
- Unicode en détail : https://unicode.org/reports/tr29/
- Conseils sur la gestion des exceptions : https://docs.microsoft.com/en-us/dotnet/standard/exceptions/
