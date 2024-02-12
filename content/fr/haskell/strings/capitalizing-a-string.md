---
title:                "Mettre en majuscule une chaîne"
aliases:
- /fr/haskell/capitalizing-a-string.md
date:                  2024-02-03T19:05:20.716714-07:00
model:                 gpt-4-0125-preview
simple_title:         "Mettre en majuscule une chaîne"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Mettre une chaîne en capitale implique de transformer la première lettre d'une chaîne donnée en majuscule tout en s'assurant que les autres lettres restent en minuscule. Les programmeurs font cela pour formater les sorties, adhérer à la correction grammaticale dans les textes, ou améliorer la lisibilité des données générées.

## Comment faire :
En Haskell, vous pouvez mettre une chaîne en capitale en utilisant la bibliothèque standard sans avoir besoin de bibliothèques tierces.

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- Exemple d'utilisation :
main = putStrLn $ capitalize "hello world"
```

Sortie :
```
Hello world
```

Pour des scénarios plus complexes ou pour plus de facilité d'usage, vous pourriez vouloir utiliser une bibliothèque tierce comme `text`, qui est populaire pour la manipulation efficace de chaînes de caractères en Haskell.

D'abord, vous devez ajouter `text` aux dépendances de votre projet. Ensuite, vous pouvez utiliser ses fonctions pour mettre en capitale une chaîne de caractères comme suit :

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- Exemple d'utilisation avec la bibliothèque text :
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

Sortie :
```
Hello world
```

Ces deux exemples montrent des moyens simples mais efficaces de mettre une chaîne en capitale en Haskell, avec ou sans bibliothèques tierces.
