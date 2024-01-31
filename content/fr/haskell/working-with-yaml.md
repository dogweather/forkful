---
title:                "Travailler avec YAML"
date:                  2024-01-19
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, ou "YAML Ain't Markup Language", est un format de données lisible par l'homme pour la configuration des applications. Les programmeurs l'utilisent souvent pour gérer les paramètres, stocker des informations de manière structurée et faciliter l'intégration avec divers outils et langages, dont Haskell.

## How to:
Pour travailler avec YAML en Haskell, on utilise la bibliothèque 'yaml'. Voici comment la parser et générer :

```Haskell
-- N'oubliez pas d'ajouter yaml à votre fichier .cabal ou package.yaml
import Data.Yaml

-- Un exemple simple de parsing de YAML
main :: IO ()
main = do
    yamlData <- decodeFileEither "config.yaml"
    case yamlData of
        Left e -> print e
        Right config -> print (config :: Value)

-- Générer du YAML
myData :: Value
myData = object ["name" .= "John Doe", "age" .= 30]

main :: IO ()
main = print (encode myData)
```

## Deep Dive
YAML est né en 2001, pensé comme un langage plus lisible que le XML. En Haskell, outre 'yaml', on trouve des alternatives comme 'aeson-yaml' pour ceux habitués à aeson pour le JSON. Le parsing en Haskell est facilité par une conversion interne aux types du langage, permettant des vérifications de type et une manipulation aisée.

## See Also
- Documentation de la bibliothèque 'yaml': https://hackage.haskell.org/package/yaml
- Tutoriel YAML par exemple : https://learnxinyminutes.com/docs/yaml/
- Comparaison YAML et JSON: https://phoenixnap.com/kb/yaml-vs-json
