---
title:                "Trouver la longueur d'une chaîne de caractères"
html_title:           "Haskell: Trouver la longueur d'une chaîne de caractères"
simple_title:         "Trouver la longueur d'une chaîne de caractères"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?

Trouver la longueur d'une chaîne de caractères est une tâche courante en programmation, qui consiste à compter le nombre de caractères dans une chaîne donnée. Les programmeurs le font pour différents besoins, tels que la validation de données, le traitement de texte, ou simplement pour des statistiques.

## Comment faire:

Dans Haskell, il existe plusieurs façons de trouver la longueur d'une chaîne de caractères. Voici deux exemples, avec leur code et sortie correspondante:

```Haskell
-- Exemple 1: Utilisation de la fonction prédéfinie 'length'
length "Bonjour" -- Sortie: 7

-- Exemple 2: Utilisation d'une fonction récursive
-- Définition de la fonction 'longueur' prenant une chaîne de caractères en paramètre
longueur :: String -> Int
longueur "" = 0 -- Cas de base: la chaîne est vide, la longueur est 0
longueur (_:xs) = 1 + longueur xs -- Cas récursif: la longueur est 1 plus la longueur du reste de la chaîne

longueur "Bonjour" -- Sortie: 7
```

## Plongée en profondeur:

Trouver la longueur d'une chaîne de caractères est un concept qui existe depuis les débuts de la programmation. Certaines langues de programmation telles que Java ont des fonctions intégrées pour cette tâche, tandis que d'autres comme Haskell utilisent des fonctions récursives pour calculer la longueur.

Il existe également des alternatives à la méthode récursive présentée, telles que l'utilisation de la fonction `foldr` ou la compréhension de liste. Cependant, ces méthodes ne sont pas aussi efficaces que la récursion pour des chaînes de caractères de grande taille.

En termes d'implémentation, la fonction `length` en Haskell utilise une technique appelée "strictness analysis" pour optimiser les performances. Elle consiste à forcer la valeur de la longueur à être évaluée à chaque itération, plutôt que d'attendre la fin. Cela peut sembler inutile pour de petits exemples, mais pour des chaînes de caractères plus importantes, cela peut faire une différence significative en termes de temps d'exécution.

## Voir aussi:

- [Documentation Haskell pour la fonction 'length'](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:length)
- [Exemples de fonctions récursives en Haskell](https://wiki.haskell.org/Recursion)
- [Une explication détaillée de la "strictness analysis" en Haskell](https://stackoverflow.com/questions/20501042/strictness-analysis-in-haskell)