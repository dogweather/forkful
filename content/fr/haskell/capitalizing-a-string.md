---
title:                "Mettre une chaîne de caractères en majuscules"
html_title:           "C: Mettre une chaîne de caractères en majuscules"
simple_title:         "Mettre une chaîne de caractères en majuscules"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Capitaliser une chaîne, c'est convertir les premières lettres de chaque mot en majuscules. Les programmeurs utilisent cette technique pour uniformiser les données textuelles ou pour respecter les conventions typographiques.

## Comment ça marche :

```Haskell
import Data.Char(toUpper)

-- Capitalize the first letter of each word
capitalize :: String -> String
capitalize = unwords . map (\(x:xs) -> toUpper x : xs) . words

-- Usage
main = putStrLn $ capitalize "bonjour, comment ça va ?"

-- Output:
-- "Bonjour, Comment ça Va ?"
```

## Plongée en profondeur
Capitaliser des chaînes est un concept qui remonte à l'époque des premières machines à écrire et de la typographie, servant à mettre en avant des noms propres et des début de phrases. En Haskell, l'approche typique comprend la fonction `words` pour découper la chaîne en mots, et `map` pour appliquer la capitalisation à chaque mot. La fonction `toUpper` de `Data.Char` est standard pour la conversion en majuscule. 

Des alternatives incluent l'utilisation de bibliothèques telles que `text` ou `bytestring` pour gérer de grands volumes de données plus efficacement. Au sujet de l'implémentation, `toUpper` gère déjà les caractères accentués en conformité avec les standards Unicode, ce qui est crucial pour les langues comme le français.

## Voir également

- Haskell `Data.Char` documentation: https://hackage.haskell.org/package/base/docs/Data-Char.html
- Article sur Unicode et Haskell: https://wiki.haskell.org/Unicode_input
- Documentation de la bibliothèque `text`: https://hackage.haskell.org/package/text
