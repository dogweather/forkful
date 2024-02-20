---
date: 2024-01-20 17:47:25.569033-07:00
description: "La longueur d'une cha\xEEne, c'est compter les \xE9l\xE9ments qui la\
  \ composent. Les programmeurs le font pour valider des donn\xE9es, optimiser des\
  \ performances ou\u2026"
lastmod: 2024-02-19 22:05:16.562278
model: gpt-4-1106-preview
summary: "La longueur d'une cha\xEEne, c'est compter les \xE9l\xE9ments qui la composent.\
  \ Les programmeurs le font pour valider des donn\xE9es, optimiser des performances\
  \ ou\u2026"
title: "Trouver la longueur d'une cha\xEEne de caract\xE8res"
---

{{< edit_this_page >}}

## What & Why?
La longueur d'une chaîne, c'est compter les éléments qui la composent. Les programmeurs le font pour valider des données, optimiser des performances ou simplement manipuler du texte.

## How to:
En Haskell, pour trouver la longueur d'une chaîne, on utilise la fonction `length`. Simple et directe, voici comment ça fonctionne :

```haskell
main :: IO ()
main = do
    let message = "Bonjour, monde!"
    print $ length message
```

Sortie : `15`

## Deep Dive
Historiquement, la fonction `length` fait partie des fonctions de base en Haskell, traitant les listes depuis le début. En Haskell, une chaîne de caractères est une liste de caractères (`[Char]`), donc `length` compte efficacement les `Char` dans la liste.

Alternatives? Oui, pour les chaînes UTF-8, la longueur n'est pas si simple; des caractères peuvent prendre plus d'un octet. On utilise alors `Data.Text` avec `Data.Text.length`, surtout pour du texte non ASCII. 

Détails d'implémentation : `length` travaille en parcourant toute la liste, donc elle est linéaire en complexité temporelle - O(n). Pour les grosses listes, ça peut peser, alors, réfléchis bien avant de l'utiliser sur de très grandes chaînes.

## See Also
Pour plus d’informations, jetez un œil ici :
- La documentation de `length` sur Hackage : [Hackage: length](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-List.html#v:length)
- Sur `Data.Text` et le traitement de texte en Haskell : [Hackage: Data.Text](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)
