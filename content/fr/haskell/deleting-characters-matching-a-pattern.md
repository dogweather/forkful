---
title:    "Haskell: Suppression de caractères correspondant à un motif"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un motif peut être utile lors de la manipulation de chaînes de caractères dans un programme Haskell. Cela peut être particulièrement utile pour nettoyer des données ou pour effectuer des opérations spécifiques sur une chaîne de caractères.

## Comment faire

Pour supprimer des caractères correspondant à un motif dans Haskell, nous pouvons utiliser la fonction `filter` en combinaison avec une fonction prédicative. Par exemple, si nous voulons supprimer tous les espaces d'une chaîne de caractères, nous pouvons utiliser le code suivant :

```Haskell
filter (/= ' ') "Bonjour, comment ça va ?"
```

Cela renverra la chaîne de caractères "Bonjour,commentçava?" en supprimant tous les espaces. Nous pouvons également utiliser des fonctions comme `isDigit` ou `isUpper` pour supprimer des caractères numériques ou majuscules, respectivement.

## Plongée en profondeur

L'utilisation de la fonction `filter` pour supprimer des caractères correspondant à un motif peut sembler simple, mais il est important de comprendre comment la fonction prédicative fonctionne réellement. Lorsque nous passons une fonction prédicative à `filter`, elle est appliquée à chaque élément de la liste et renvoie `True` ou `False`. Si la fonction renvoie `True`, l'élément est conservé, sinon il est supprimé de la liste finale.

De plus, il est important de noter que la fonction `filter` renvoie une liste et non une chaîne de caractères. Cela signifie que si nous voulons obtenir une chaîne de caractères en sortie, nous devons utiliser la fonction `concat` pour concaténer les éléments de la liste en une seule chaîne de caractères.

## Voir aussi

- La documentation sur la fonction `filter` dans la [bibliothèque standard de Haskell](https://hackage.haskell.org/package/base-4.14.1.0/docs/src/GHC.List.html#filter).
- Un tutoriel sur les opérations de manipulation de chaînes de caractères en Haskell : [haskell.org/tutorial/strings](https://www.haskell.org/tutorial/strings.html).
- Un exemple pratique d'utilisation de la fonction `filter` pour nettoyer des données : [How to Clean Data with Haskell and Csv](https://www.ingowald.com/journal/How-to-clean-data-with-Haskell-and-Csv.html).