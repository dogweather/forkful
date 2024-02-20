---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:41.609255-07:00
description: "Travailler avec des fichiers CSV (Valeurs S\xE9par\xE9es par des Virgules)\
  \ implique d'analyser et de g\xE9n\xE9rer des fichiers qui stockent des donn\xE9\
  es tabulaires\u2026"
lastmod: 2024-02-19 22:05:16.594343
model: gpt-4-0125-preview
summary: "Travailler avec des fichiers CSV (Valeurs S\xE9par\xE9es par des Virgules)\
  \ implique d'analyser et de g\xE9n\xE9rer des fichiers qui stockent des donn\xE9\
  es tabulaires\u2026"
title: Travailler avec CSV
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Travailler avec des fichiers CSV (Valeurs Séparées par des Virgules) implique d'analyser et de générer des fichiers qui stockent des données tabulaires dans un format texte simple. Les programmeurs s'engagent fréquemment dans cette tâche pour importer ou exporter efficacement des données depuis des feuilles de calcul, des bases de données, ou pour faciliter l'échange de données entre différents programmes.

## Comment faire :

En Haskell, la gestion des fichiers CSV peut être réalisée en utilisant la bibliothèque `cassava`, l'une des bibliothèques tierces populaires à cette fin. Ci-dessous, des exemples illustrant comment lire et écrire dans des fichiers CSV en utilisant `cassava`.

**1. Lire un fichier CSV :**

Tout d'abord, assurez-vous d'avoir `cassava` installé en l'ajoutant au fichier cabal de votre projet ou en utilisant Stack.

Voici un exemple simple pour lire un fichier CSV et imprimer chaque enregistrement. Nous supposons que le fichier CSV a deux colonnes : nom et âge.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "people.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, age) ->
            putStrLn $ name ++ " a " ++ show (age :: Int) ++ " ans."
```

En supposant que `people.csv` contient :
```
John,30
Jane,25
```
Le résultat sera :
```
John a 30 ans.
Jane a 25 ans.
```

**2. Écrire un fichier CSV :**

Pour créer un fichier CSV, vous pouvez utiliser la fonction `encode` de `cassava`.

Voici comment vous pourriez écrire une liste d'enregistrements dans un fichier CSV :

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

Après l'exécution de ce programme, `output.csv` contiendra :

```
John,30
Jane,25
```

Cette introduction concise au travail avec des fichiers CSV en Haskell en utilisant la bibliothèque `cassava` démontre comment lire et écrire dans des fichiers CSV, rendant les tâches de manipulation de données plus accessibles pour ceux qui sont nouveaux dans le langage.
