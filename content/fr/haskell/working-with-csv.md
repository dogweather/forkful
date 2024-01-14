---
title:                "Haskell: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Pourquoi

CSV, ou Comma Separated Values, est un format de fichier utilisé pour stocker des données tabulaires, telles que des feuilles de calcul ou des bases de données. Travailler avec des fichiers CSV peut être utile pour importer et exporter des données entre différentes applications, ou pour effectuer des analyses de données. Dans cet article, nous allons explorer comment travailler avec des fichiers CSV en Haskell.

## Comment Faire

Pour travailler avec des fichiers CSV en Haskell, nous devons d'abord importer le module `Data.Csv`, qui se trouve dans la bibliothèque `cassava`. Ensuite, nous pouvons utiliser la fonction `decode` pour lire un fichier CSV et stocker les données dans une structure de données appelée `Either String (Vector (Vector ByteString))`. Voici un exemple de code pour lire un fichier CSV et afficher son contenu :

```Haskell
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  csvData <- BL.readFile "chemin/vers/mon/fichier.csv"
  let parsedData = decode NoHeader csvData :: Either String (Vector (Vector ByteString))
  case parsedData of
    Left err -> putStrLn "Une erreur s'est produite lors de la lecture du fichier"
    Right data -> V.mapM_ print data
```

Dans cet exemple, nous utilisons la fonction `readFile` de la bibliothèque `Data.ByteString.Lazy` pour lire le contenu du fichier CSV en tant que `ByteString`. Ensuite, nous utilisons la fonction `decode` avec le paramètre `NoHeader` pour indiquer que le fichier CSV n'a pas d'en-têtes de colonne. Enfin, nous utilisons la fonction `mapM_` de la bibliothèque `Data.Vector` pour afficher chaque ligne du fichier.

Pour écrire des données dans un fichier CSV, nous pouvons utiliser la fonction `encode` de la bibliothèque `Data.Csv`. Voici un exemple de code pour écrire des données dans un fichier CSV :

```Haskell
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
  let csvData = encode $ V.fromList [["nom", "âge"], ["Jean", "30"], ["Marie", "25"]]
  BL.writeFile "chemin/vers/mon/fichier.csv" csvData
```

Dans cet exemple, nous utilisons la fonction `encode` pour convertir une structure de données de type `Vector (Vector String)` en `ByteString` au format CSV, puis nous utilisons la fonction `writeFile` pour écrire ces données dans un fichier.

## Plongée en Profondeur

Bien que les exemples précédents soient simples, travailler avec des fichiers CSV peut être plus complexe selon le contenu du fichier. Par exemple, si le fichier contient des valeurs numériques, nous devrons utiliser des fonctions pour convertir ces valeurs en types appropriés en Haskell. Les fichiers CSV peuvent également contenir des en-têtes de colonne, ce qui peut simplifier la lecture et l'écriture des données.

Il est également important de prendre en compte les caractères spéciaux, tels que les guillemets et les virgules, qui peuvent être présents dans les données CSV. Dans ce cas, nous devrons utiliser des fonctions de la bibliothèque `Data.Text` pour traiter correctement ces caractères.

Enfin, nous pouvons utiliser des fonctions de tri et de filtrage de la bibliothèque `Data.Vector` pour manipuler facilement les données dans un fichier CSV.

## Voir Aussi

- [Documentation de la bibliothèque cassava](https://hackage.haskell.org/package/cassava)
- [Guide officiel pour travailler avec les fichiers CSV en Haskell](https://wiki.haskell.org/Working_with_CSV_files)