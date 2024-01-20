---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

---

# Découverte de Haskell: Trouver la Longueur d'une Chaîne de Caractères

## Qu'est-ce que c'est et Pourquoi?

Trouver la longueur d'une chaîne signifie déterminer le nombre de caractères dans cette chaîne. Pourquoi? Simplement parce que c'est une opération fondamentale en programmation, utile pour tout, du traitement des données à l'interface utilisateur.

## Comment faire:

En Haskell, c'est tout simple. Utilisez la fonction `length`. Regardons quelques exemples:

```Haskell
longueur :: String -> Int
longueur = length

main :: IO ()
main = do
  print (longueur "")
  print (longueur "Bonjour")
```
La sortie sera:

```Haskell
0
7
```
On utilise ainsi la fonction `length` pour obtenir la longueur d'une chaîne de caractères.

## Plongeon Profond:

Historiquement, la fonction `length` existe depuis les débuts de Haskell. C'est un outil robuste. Néanmoins, il est vital de se rappeler que `length` travaille avec des listes, et en Haskell, les chaînes sont des listes de caractères. Ainsi, l'utilisation de `length` sur des chaînes de grande taille peut être coûteuse en termes de performances.

Parmi les alternatives, vous pouvez convertir votre chaîne en structure de données plus adaptée comme `Text` ou `ByteString` qui offre une fonction pour obtenir la longueur plus rapidement.

## Voir Aussi:

Pour approfondir vos connaissances sur la manipulation de chaînes de caractères en Haskell, consultez ces liens:

- Le livre "Learn You a Haskell for Great Good" : http://learnyouahaskell.com
- La documentation Hackage de `Data.Text` : http://hackage.haskell.org/package/text
- La documentation Hackage de `Data.ByteString` : http://hackage.haskell.org/package/bytestring