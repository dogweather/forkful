---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Supprimer des caractères correspondant à un motif ("pattern matching") en programmation sert à filtrer les informations inutiles. C'est généralement fait pour simplifier les données ou pour extraire des informations spécifiques.

## Comment Faire :

On utilise la fonction ‘filter’ de Haskell pour supprimer les caractères correspondant à un motif. La fonction 'filter' prend une fonction Boolean et une liste comme arguments et renvoie une liste de tous les éléments pour lesquels la fonction Boolean renvoie True.

```Haskell
import Data.Char(isDigit)
effacer = filter (not . isDigit)
main :: IO ()
main = putStrLn $ effacer "abc123"
```

Le programme ci-dessus supprime tous les chiffres de la chaîne de caractères. Il affiche :

```Haskell
abc
```

## Plongée Profonde :

Historiquement, le filtrage par motif est une technique centrale en programmation fonctionnelle et a été originellement utilisé dans le langage de programmation ML. En Haskell, les alternatives pour supprimer les caractères correspondent à un motif impliquent l'utilisation de listes comprehension ou de folds.

Du point de vue de l'implémentation, Haskell utilise des arbres de décision pour le filtrage par motif. Ces arbres sont optimisés pour effectuer le minimum de tests et d'accès aux données.

## À Voir Également :

- Documentation du Haskell : https://www.haskell.org/documentation/
- Projet Haskell: https://www.haskell.org/onlinereport/haskell2010/
- Conseils sur le filtrage de motifs en Haskell : https://kowainik.github.io/posts/haskell-mini-patterns
- Fonctionnement interne du filtrage de motifs : https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/p29-sulzmann.pdf