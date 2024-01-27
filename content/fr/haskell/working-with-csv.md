---
title:                "Manipulation des fichiers CSV"
date:                  2024-01-19
html_title:           "Bash: Manipulation des fichiers CSV"
simple_title:         "Manipulation des fichiers CSV"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Manipuler des fichiers CSV (Comma-Separated Values), c'est travailler avec des données tabulaires simples. Les programmeurs le font pour importer, exporter, et traiter des données de manière interopérable.

## How to:
Utilisons `cassava`, une bibliothèque populaire pour gérer les CSV en Haskell. D'abord, installez le package avec cabal:

```shell
cabal update
cabal install cassava
```

Voici un exemple de lecture d'un fichier CSV et d'affichage de son contenu.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "data.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (nom, age) ->
            putStrLn $ nom ++ " a " ++ show age ++ " ans"
```

Si "data.csv" contient:

```
Jean,30
Marie,22
```

La sortie sera:

```
Jean a 30 ans
Marie a 22 ans
```

## Deep Dive
Le format CSV a été créé dans les années 70 et est depuis devenu un standard de facto pour l'échange de données. `cassava` est une alternative aux outils plus basiques comme `read` ou `writeFile`, offrant une API plus détaillée et une gestion des erreurs supérieure. Le parsing est réalisé grâce à des instances de `FromRecord` pour la lecture, et `ToRecord` pour l'écriture.

## See Also
- [cassava sur Hackage](https://hackage.haskell.org/package/cassava)
- [Spécification CSV RFC 4180](https://tools.ietf.org/html/rfc4180)
- [Tutoriel sur la lecture de fichiers CSV en Haskell](https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/)
