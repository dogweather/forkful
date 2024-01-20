---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

La recherche et le remplacement de texte permettent de trouver une séquence spécifique dans une chaîne de caractères et de la remplacer par une autre, ce qui est essentiel pour la manipulation de données en programmation.

## Comment faire :

Voici un exemple de comment rechercher et remplacer du texte en Haskell :
```Haskell 
import Data.List.Utils

main = do
   let startString = "Bonjour, le monde!"
   let endString = replace "le monde" "France" startString
   print endString
```

Quand vous exécuterez ce code, la sortie sera :
```Haskell
"Bonjour, France!"
```

## Vue d'ensemble :

Historiquement, les opérations de recherche et remplacement de texte sont ancrées dans les éditeurs de texte et les langages de programmation depuis les années 1970. En Haskell, cet objectif est souvent accompli avec la fonction `replace` du module `Data.List.Utils`.

Alternativement, vous pouvez réaliser cela manuellement en utilisant des fonctions de base de la liste Haskell comme `splitOn` et `intercalate`. La première fonction divise une chaîne à chaque occurrence d'un séparateur donné, et la seconde joint une liste de chaînes avec un séparateur donné.

Sur la question de l'implémentation, la fonction `replace` utilise un algorithme simple qui parcourt la chaîne de caractères, cherchant l’occurrence du motif et la remplaçant par le substitut.

## Voir aussi :

Pour un approfondissement, vous pouvez consulter ces ressources supplémentaires :

[Non-greedy string replacement in Haskell](https://stackoverflow.com/questions/16281976/non-greedy-string-replacement-in-haskell)

[Haskell for all: String diagrams, split, and intercalate](http://www.haskellforall.com/2013/12/string-diagrams.html)