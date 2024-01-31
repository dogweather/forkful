---
title:                "Trouver la longueur d'une chaîne de caractères"
date:                  2024-01-20T17:48:08.466096-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trouver la longueur d'une chaîne de caractères"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Calculer la longueur d'une chaîne, c'est simplement compter le nombre de caractères qu'elle contient. Les programmeurs le font pour valider des données, délimiter des traitements, ou simplement manipuler du texte efficacement.

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
