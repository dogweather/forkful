---
title:                "Haskell: Extraction de sous-chaînes"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Avez-vous déjà eu besoin de trouver des sous-chaînes spécifiques dans une chaîne de texte en Haskell? Peut-être vouliez-vous extraire un numéro de téléphone ou une adresse e-mail d'une grande quantité de texte. Dans cet article, nous allons expliquer comment extraire efficacement des sous-chaînes en utilisant Haskell, et pourquoi cela peut être utile dans vos projets de programmation.

## Comment faire

En Haskell, il existe plusieurs façons d'extraire des sous-chaînes d'une chaîne de texte. La première méthode consiste à utiliser la fonction `take` pour extraire un nombre spécifique de caractères à partir du début de la chaîne. Par exemple, si nous voulons extraire les 3 premiers caractères d'une chaîne, nous pouvons écrire:

```Haskell
take 3 "Hello World" -- renvoie "Hel"
```

Nous pouvons également utiliser la fonction `drop` pour sauter un certain nombre de caractères à partir du début de la chaîne. Par exemple, si nous voulons extraire tout sauf les 3 premiers caractères d'une chaîne, nous pouvons écrire:

```Haskell
drop 3 "Hello World" -- renvoie "lo World"
```

Pour extraire une sous-chaîne basée sur une position spécifique dans une chaîne, nous pouvons utiliser la fonction `takeWhile` ou `dropWhile`. Par exemple, si nous voulons extraire les caractères avant la première espace dans une chaîne, nous pouvons écrire:

```Haskell
takeWhile (/= ' ') "Hello World" -- renvoie "Hello"
```

Enfin, nous pouvons également utiliser la fonction `splitAt` pour diviser une chaîne en deux parties à un certain index. Par exemple, si nous voulons diviser une chaîne en deux parties après le troisième caractère, nous pouvons écrire:

```Haskell
splitAt 3 "Hello World" -- renvoie ("Hel", "lo World")
```

## Plongée profonde

Maintenant que nous avons vu quelques exemples de fonctions utiles pour extraire des sous-chaînes, examinons de plus près comment elles fonctionnent réellement. En Haskell, les chaînes de texte sont représentées en utilisant des listes de caractères. Cela signifie que les fonctions que nous utilisons pour extraire des sous-chaînes peuvent également être utilisées sur d'autres types de données représentés sous forme de listes.

De plus, certaines des fonctions que nous avons mentionnées, comme `takeWhile` et `dropWhile`, prennent une fonction en argument. Cela signifie que nous pouvons utiliser une fonction personnalisée pour filtrer les caractères que nous voulons extraire ou ignorer. Par exemple, si nous voulons extraire uniquement les nombres d'une chaîne, nous pouvons écrire une fonction comme ceci:

```Haskell
onlyNumbers :: Char -> Bool
onlyNumbers c = c >= '0' && c <= '9'

takeWhile onlyNumbers "12345Hello" -- renvoie "12345"
```

## Voir aussi

Maintenant que vous avez une meilleure compréhension de la façon d'extraire des sous-chaînes en utilisant Haskell, voici quelques autres ressources que vous pourriez trouver utiles:

- [Documentation officielle de Haskell sur les listes](https://www.haskell.org/onlinereport/standard-prelude.html#t:list)
- [Un tutoriel sur les opérations de base sur les chaînes de texte en Haskell](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Exemples de code pour l'utilisation de fonctions d'extraction de sous-chaînes en Haskell](https://wiki.haskell.org/Extracting_substrings)