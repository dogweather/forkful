---
title:                "Haskell: Convertir une chaîne en minuscule"
simple_title:         "Convertir une chaîne en minuscule"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Pourquoi
Il y a plusieurs raisons pour lesquelles vous pourriez vouloir convertir une chaîne de caractères en lettres minuscules en utilisant Haskell. Une des raisons pourrait être pour normaliser les données avant de les comparer ou pour rendre l'affichage plus cohérent.

# Comment faire
Pour convertir une chaîne de caractères en lettres minuscules en Haskell, vous pouvez utiliser la fonction `toLower` du module `Data.Char`. Cela peut être fait en utilisant la structure suivante dans votre code :

```
import Data.Char (toLower)

-- Définition de la fonction de conversion
convertToLower :: String -> String
convertToLower str = map toLower str

-- Exemple d'utilisation
main = print (convertToLower "Haskell est super")
```

La sortie de ce code sera `haskell est super`, où toutes les lettres ont été converties en minuscules. Vous pouvez également utiliser la fonction `toLower` pour convertir chaque caractère individuellement. Par exemple :

```
main = print (toLower 'A')
```

Cela produira `a` comme sortie.

# Plongée profonde
Il est important de noter que la fonction `toLower` ne fonctionne que pour les caractères de l'alphabet anglais. Si vous avez besoin de convertir des caractères d'autres langues en lettres minuscules, vous devrez utiliser une autre méthode. Vous pouvez également combiner la fonction `toLower` avec d'autres fonctions du module `Data.Char` pour manipuler davantage vos données.

# Voir aussi
- [Documentation officielle du module Data.Char](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Guide de référence rapide de Haskell pour convertir des chaînes de caractères](http://bfpg.github.io/haskell/linguistics/2015/03/03/haskell-quicksheet-string-and-bytestring-case-conversion.html)