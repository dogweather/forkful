---
title:                "Travailler avec YAML"
html_title:           "Haskell: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire ?
Travailler avec YAML, c'est manipuler des données structurées dans un format facilement lisible par les humains et les machines. Les programmeurs l'utilisent souvent pour stocker des configurations de logiciel ou des données de configuration pour des applications web.

## Comment faire :
Voici quelques exemples de code pour lire et écrire des fichiers YAML en Haskell :

```Haskell
import Data.Yaml
import qualified Data.ByteString.Char8 as C

-- Lecture :
yamlData <- C.readFile "config.yaml"
let parsedData = decode yamlData :: Maybe Data.Yaml.Value

-- Écriture :
let data = "key: value"
C.writeFile "output.yaml" $ encode data
```

La librairie `Data.Yaml` fournit des fonctions pratiques pour manipuler les données YAML dans Haskell. Il est également possible d'utiliser des librairies de parsing de JSON pour travailler avec YAML, car ces formats sont étroitement liés.

## Plongée en profondeur :
YAML est un format de données qui a été créé en 2001 pour offrir une alternative plus humaine aux formats de données existants tels que XML et JSON. Elle est similaire à ces derniers en termes de performance, mais elle est conçue pour être plus facile à lire et à écrire pour les humains. Les alternatives à YAML incluent des formats tels que TOML et INI.

## À voir :
Consultez la documentation officielle de la librairie `Data.Yaml` pour en savoir plus sur son utilisation en Haskell : https://hackage.haskell.org/package/yaml.

Pour comparer YAML avec d'autres formats de données, consultez cette présentation sur TOML : https://hackage.haskell.org/package/toml.