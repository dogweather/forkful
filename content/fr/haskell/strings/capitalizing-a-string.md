---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:20.716714-07:00
description: "Comment faire : En Haskell, vous pouvez mettre une cha\xEEne en capitale\
  \ en utilisant la biblioth\xE8que standard sans avoir besoin de biblioth\xE8ques\
  \ tierces."
lastmod: '2024-03-13T22:44:57.815801-06:00'
model: gpt-4-0125-preview
summary: "En Haskell, vous pouvez mettre une cha\xEEne en capitale en utilisant la\
  \ biblioth\xE8que standard sans avoir besoin de biblioth\xE8ques tierces."
title: "Mettre en majuscule une cha\xEEne"
weight: 2
---

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
