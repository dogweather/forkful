---
date: 2024-01-20 17:48:08.466096-07:00
description: "Calculer la longueur d'une cha\xEEne, c'est simplement compter le nombre\
  \ de caract\xE8res qu'elle contient. Les programmeurs le font pour valider des donn\xE9\
  es,\u2026"
lastmod: '2024-03-13T22:44:58.034008-06:00'
model: gpt-4-1106-preview
summary: "Calculer la longueur d'une cha\xEEne, c'est simplement compter le nombre\
  \ de caract\xE8res qu'elle contient."
title: "Trouver la longueur d'une cha\xEEne de caract\xE8res"
weight: 7
---

## How to:
Pour obtenir la longueur d'une chaîne en PowerShell, on utilise la propriété `.Length`. Voici un exemple :

```PowerShell
$maChaine = "Bonjour, le monde!"
$longueur = $maChaine.Length
$longueur  # Affiche la longueur
```

Sortie:

```
17
```

Si la chaîne est vide, elle retournera `0` :

```PowerShell
$chaineVide = ""
$longueurVide = $chaineVide.Length
$longueurVide  # Affiche 0
```

Sortie:

```
0
```

## Deep Dive
Avant PowerShell, des langages comme VBScript utilisaient des fonctions comme `Len()` pour obtenir la taille d'une chaîne. En PowerShell, on accède directement à la propriété `.Length`, qui est héritée du type .NET `System.String`. En plus de `.Length`, on peut utiliser d'autres méthodes pour travailler avec les chaînes, comme `.Substring()`, `.Replace()`, etc.

Si la performance est critique, il faut savoir que l'accès à la propriété `.Length` est très rapide car la longueur de la chaîne est stockée avec l'objet chaîne dans la mémoire. Donc, pas de calcul à chaque appel, juste une lecture de valeur.

## See Also
- Documentation de .NET sur `System.String` : [System.String](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-6.0)
