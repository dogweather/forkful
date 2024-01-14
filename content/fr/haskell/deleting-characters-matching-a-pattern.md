---
title:                "Haskell: Supprimer les caractères correspondant à un motif"
simple_title:         "Supprimer les caractères correspondant à un motif"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un motif peut être utile dans de nombreux cas, comme la manipulation de chaînes de caractères ou la modification de données. Cela peut également permettre d'épurer des données inexploitables ou de supprimer des caractères indésirables d'un fichier texte.

## Comment faire

Voici un exemple de code en Haskell pour supprimer les caractères correspondant à un motif donné :

```Haskell
deletePattern :: String -> String
deletePattern [] = []
deletePattern (x:xs)
    | x == 'a' = deletePattern xs
    | otherwise = x : deletePattern xs
```
Output : Supposons que notre chaîne de caractères soit "labbab" et que nous voulions supprimer tous les caractères "a" :
```
Resultat: "lbbb"
```
Ce code utilise une fonction récursive pour parcourir chaque caractère de la chaîne et le supprimer s'il correspond au motif donné. Il est possible de mettre en place d'autres conditions pour supprimer différents caractères ou pour prendre en compte des motifs plus complexes.

## Zoom sur la suppression de caractères correspondant à un motif

Supprimer des caractères correspondant à un motif peut également être utilisé dans le cadre de la programmation fonctionnelle pour effectuer des transformations sur des données. En utilisant des fonctions de filtrage et de transformation, il est possible de supprimer et même de remplacer des caractères en fonction de différents motifs.

Il est également important de noter qu'il existe plusieurs façons de supprimer des caractères correspondant à un motif en Haskell, en utilisant entre autres les fonctions standard `filter` ou `delete` de la librairie Data.List.

## Voir aussi

- [Documentation de Haskell](https://www.haskell.org/documentation/)
- [Les fonctions de transformation en Haskell](https://wiki.haskell.org/Functional_programming)
- [Utilisation de `filter` pour supprimer des éléments d'une liste en Haskell](https://www.geeksforgeeks.org/haskell-filter/)
- [Utilisation de `delete` pour éliminer des éléments d'une liste en Haskell](https://www.geeksforgeeks.org/haskell-delete/)