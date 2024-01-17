---
title:                "Travailler avec les fichiers csv"
html_title:           "Haskell: Travailler avec les fichiers csv"
simple_title:         "Travailler avec les fichiers csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Le CSV (Comma Separated Values) est un format de fichier utilisé pour stocker des données sous forme de tableaux avec des valeurs séparées par des virgules. Les programmeurs travaillent souvent avec des fichiers CSV car ils sont faciles à lire et à écrire, et peuvent être importés dans de nombreux programmes et applications pour analyse et traitement des données.

## Comment faire:

Pour travailler avec des fichiers CSV en Haskell, vous aurez besoin d'importer le module ```Data.Csv``` dans votre code. Voici un exemple de code qui lit un fichier CSV et affiche chaque ligne avec son index:

```Haskell
import Data.Csv (decode, HasHeader(NoHeader))
import qualified Data.ByteString.Lazy as ByteString
import System.IO

inputFile = "data.csv"

main = do
   contents <- ByteString.readFile inputFile
   case decode NoHeader contents of
      Left _ -> putStrLn "Erreur lors de la lecture du fichier"
      Right rows -> zipWithM_ (\n row -> putStrLn (show n ++ ". " ++ show row)) [1..] rows
```

Output:
```
1. ["John", "Doe", "30", "Engineer"]
2. ["Jane", "Smith", "25", "Developer"]
3. ["Bob", "Johnson", "45", "Manager"]
```

## Plongée en profondeur:

Les fichiers CSV ont été inventés dans les années 1970 pour faciliter l'échange de données entre différentes applications. Bien qu'ils soient pratiques, ils peuvent également être source d'erreurs et de problèmes de compatibilité en raison de leur structure non normalisée. Alternativement, certains programmeurs préfèrent utiliser des formats de fichiers dédiés à l'échange de données tels que JSON ou XML.

L'implémentation du module ```Data.Csv``` est basée sur le type de données ```Record``` qui représente une ligne de données dans un fichier CSV. Il est important de noter que le type de données est paramétré par un type fantôme défini dans le type classe ```HasHeader```, ce qui signifie que vous devez vous assurer que le type que vous utilisez est bien conforme à la classe avant de l'utiliser.

## A voir également:

- [Documentation du module Data.Csv](http://hackage.haskell.org/package/cassava/docs/Data-Csv.html)
- [Traitement de fichiers CSV en Python](https://realpython.com/python-csv/)