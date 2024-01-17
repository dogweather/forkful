---
title:                "Travailler avec yaml"
html_title:           "Elm: Travailler avec yaml"
simple_title:         "Travailler avec yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

YAML est un langage de balisage utilisé par les programmeurs pour structurer et organiser des données. Il est populaire dans les applications web, les outils de développement logiciel et les scripts automatisés, car il est facile à lire et à écrire.

## Comment:

La syntaxe YAML utilise des indentations pour définir la structure des données. Voici un exemple en utilisant le module YAML de Elm:

```Elm
import Yaml exposing (..)

monFichier : String
monFichier =
    """
    fruits:
        - pomme
        - banane
        - fraise
    legumes:
        - carotte
        - tomate
    """

monFichierEnYaml : Result ParserError Value
monFichierEnYaml =
    parse monFichier
```

La sortie de la fonction parse sera une structure de données que vous pourrez utiliser dans votre programme. Par exemple, vous pourriez accéder au premier fruit de la liste en utilisant `monFichierEnYaml.fruits[0]`, qui retournerait "pomme".

## Plongée en profondeur:

YAML a été créé en 2001 dans le but de remplacer les fichiers de configuration XML complexes. Il est largement utilisé dans le développement logiciel pour définir des configurations, des ressources et des données structurées. Certains alternatives à YAML sont JSON, qui est également largement utilisé dans le développement web, et TOML, qui se concentre sur la lisibilité pour les humains. En utilisant le module YAML de Elm, vous utilisez en fait le package JavaScript js-yaml, qui est basé sur le langage JavaScript.

## Voir aussi:

Pour en savoir plus sur YAML et son utilisation avec Elm, veuillez consulter la documentation officielle du module YAML de Elm: https://package.elm-lang.org/packages/NoRedInk/elm-yaml/latest/

Vous pouvez également consulter ces sources pour en apprendre davantage sur YAML et les autres langages de balisage:
- https://yaml.org/
- https://www.json.org/
- https://toml.io/