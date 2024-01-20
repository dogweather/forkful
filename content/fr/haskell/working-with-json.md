---
title:                "Travailler avec json"
html_title:           "Haskell: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

JSON (JavaScript Object Notation) est un format de données utilisé pour stocker et échanger des informations structurées entre différents systèmes informatiques. En tant que programmeurs, nous travaillons souvent avec JSON car il est largement utilisé dans les applications Web et mobiles pour stocker et transmettre des données.

## Comment faire:

```Haskell
import Data.Aeson

-- Conversion de données en JSON:
let data = ["apple", "banana", "orange"]
let json = encode data

-- Conversion de JSON en données:
let json = "[1, 2, 3, 4]"
let decoded = decode json :: Maybe [Int]

-- Traitement de données JSON:
case decoded of
    Just numbers -> putStrLn (show (sum numbers))
    Nothing -> putStrLn "Impossible de décoder le JSON."
```

## Profondeur de plongée:

- JSON a été développé par Douglas Crockford dans les années 1990 en réponse à des formats de données tels que XML qui étaient difficiles à lire et à utiliser.
- Les alternatives à JSON incluent XML, YAML et TOML, mais JSON reste populaire en raison de sa simplicité et de sa compatibilité avec JavaScript.
- En Haskell, le module `Data.Aeson` fournit des fonctions pour convertir des données en JSON et vice versa. Il utilise un type de données personnalisé `Value` pour représenter des valeurs JSON et utilise des lenses pour faciliter l'accès et la manipulation des données.

## À voir:

- [Documentation officielle de Data.Aeson](https://hackage.haskell.org/package/aeson-2.0.1.0/docs/Data-Aeson.html)