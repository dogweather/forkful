---
title:                "Utiliser les expressions régulières"
html_title:           "Haskell: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi l'utiliser?

Les expressions régulières, également appelées "regex", sont un moyen de manipuler et de trouver des motifs de texte dans une chaîne de caractères. Les programmeurs les utilisent pour effectuer des opérations de recherche et de remplacement sur des données textuelles, telles que des chaînes de caractères ou des fichiers.

## Comment faire:

Voici un exemple simple d'utiliser des expressions régulières en Haskell pour trouver des adresses e-mail dans une chaîne de caractères:

```Haskell
import Text.Regex.Posix

-- Définir une expression régulière pour trouver des adresses email
regex :: Regex
regex = makeRegex "\\w+@\\w+\\.\\w+" :: Regex

-- Utiliser la fonction matchM pour trouver une adresse email dans une chaîne de caractères
testString :: String
testString = "Mon adresse email est moi@example.com"

-- Utiliser la fonction matchM pour trouver une adresse email dans une chaîne de caractères
output :: Maybe (MatchOffset, MatchLength)
output = matchM regex testString
```
La sortie de ce code serait ```Just (19,4)```, indiquant que l'adresse e-mail se trouve à partir du 19ème caractère de la chaîne avec une longueur de 4 caractères.

## Plongée en profondeur:

Les expressions régulières ont été inventées dans les années 1950 par le mathématicien américain Stephen Cole Kleene. Il a utilisé des concepts mathématiques pour créer un système de notation pour les motifs de texte.

Il existe également d'autres façons de manipuler des motifs de texte, telles que les fonctions de chaînes de caractères de base, mais les expressions régulières offrent une méthode plus puissante et plus efficace pour trouver des motifs complexes dans une chaîne.

En Haskell, les expressions régulières sont implémentées dans le module Text.Regex.Posix, qui doit être importé pour utiliser les fonctions regex.

## Voir aussi:

- [Documentation de base sur les expressions régulières en Haskell](https://hackage.haskell.org/package/regex-posix-0.95.2/docs/Text-Regex-Posix.html)
- [Cours avancé sur les expressions régulières en Haskell](https://wiki.haskell.org/Regular_expressions)
- [Démo interactive pour tester les expressions régulières en Haskell](https://regex-haskell.com/)