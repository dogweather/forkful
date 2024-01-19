---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Un tour à Haskell - Conversion de Chaînes de Caractères en Minuscule

## Quoi & Why?
La conversion de chaîne en minuscules est une transformation standard dans l'informatique où tous les caractères alphabétiques dans une chaîne sont rendus minuscules. On le fait surtout pour normaliser les données textuelles pour le traitement, la comparaison ou le tri.

## Comment faire:

```Haskell
import Data.Char (toLower)

main = do
    let inputString = "J'aime HASKELL!"
    let lowerCaseString = map toLower inputString
    putStrLn lowerCaseString
```
Cela donnera le résultat suivant:

```Haskell
j'aime haskell!
```

Comme on peut le voir, le `map` est utilisé pour appliquer la fonction `toLower` à chaque caractère de la chaîne.

## Plongée profonde

### Contexte historique:
La fonction de conversion en minuscules est une pratique standard depuis le début de la programmation, pour rendre le texte uniforme indépendamment des cas utilisés lors de la saisie de données.

### Alternatives:
La bibliothèque standard Text (Data.Text.Lazy et Data.Text) pourrait être utilisée pour des routines plus complexes ou pour une performance meilleure que celle des chaînes internes de Haskell.

### Details d'Implémentation:
La fonction `map` dans Haskell est une fonction de haut ordre qui prend une autre fonction et une liste comme arguments pour produire une nouvelle liste construite à partir de l'application de la fonction à chaque élément de la liste.

## Voir Aussi:

1. Pour plus d'informations sur la fonction `map` en Haskell, consultez la [Documentation de Hackage sur 'map'](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:map)
2. Pour comprendre la présence de conversions de cas en informatique, jetez un coup d'oeil à [Wikipedia's 'Case Folding'](https://en.wikipedia.org/wiki/Case_folding)
3. Pour une performance plus rapide pour de grandes données, utilisez [Data.Text Library](https://hackage.haskell.org/package/text)