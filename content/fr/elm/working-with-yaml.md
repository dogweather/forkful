---
title:                "Elm: Travailler avec le yaml"
simple_title:         "Travailler avec le yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Elm, vous avez sûrement entendu parler de YAML. Mais pourquoi voulez-vous vous lancer dans l'utilisation de cette syntaxe de sérialisation de données? Eh bien, YAML offre un moyen simple et lisible pour structurer les données, ce qui le rend idéal pour stocker des configurations ou des données de configuration pour une application Elm.

## Comment Faire

Commençons par un exemple simple de YAML dans Elm. Supposons que nous voulions stocker une liste de tâches avec une description et un état. Voici à quoi cela pourrait ressembler en YAML:

```Elm
tasks:
- description: "Faire la lessive"
  completed: false
- description: "Faire les courses"
  completed: true
```

Comme vous pouvez le voir, YAML utilise une syntaxe d'indentation pour définir la structure des données. Chaque élément de la liste est représenté par un tiret "-" et les paires clé-valeur sont définies par ":". Nous pouvons maintenant facilement importer ces données en Elm en utilisant le package `elm-exploration/yaml`.

```Elm
import Yaml.Decode as Yaml

-- Définir notre type de données
type alias Task =
  { description : String
  , completed : Bool
  }

-- Définir un Décodage pour nos tâches
taskDecoder : Yaml.Decoder Task
taskDecoder =
  Yaml.map2 Task
    (Yaml.field "description" Yaml.string)
    (Yaml.field "completed" Yaml.bool)

-- Charger nos données YAML à partir d'un fichier ou d'une chaîne de caractères
yaml : Result String Task
yaml =
  Yaml.fromString yamlString
    |> Result.andThen (Yaml.decodeList taskDecoder)

-- Utiliser notre résultat dans notre application
case yaml of
  Ok tasks ->
    -- Faire quelque chose avec nos tâches
  Err err ->
    -- Faire face à une erreur lors du décodage
```

Comme vous pouvez le voir, il est assez facile de travailler avec YAML en utilisant le package `elm-exploration/yaml`. Cependant, cela ne couvre que les bases. Voyons maintenant quelques autres fonctionnalités de YAML dans la section suivante.

## Plongée en Profondeur

Une autre fonctionnalité utile de YAML est la possibilité d'utiliser des ancres et des alias. Cela nous permet de réutiliser des valeurs d'un endroit à un autre dans notre fichier YAML en utilisant simplement un alias. Cela peut être utile lors de la définition de grandes configurations avec des valeurs répétées.

Par exemple, supposons que nous souhaitons stocker une configuration pour notre application Elm avec plusieurs environnements tels que développement, test et production. Voici à quoi cela pourrait ressembler en YAML en utilisant des ancres et des alias:

```Elm
default: &defaultConfig
  api: "https://api.example.com"
config:
  development:
    <<: *defaultConfig
    debug: true
  test:
    <<: *defaultConfig
    debug: false
  production:
    <<: *defaultConfig
    api: "https://api.prod.example.com"
```

Comme vous pouvez le voir, nous avons défini une ancre nommée `defaultConfig` qui contient notre configuration par défaut pour l'API. Ensuite, dans chaque environnement, nous avons utilisé l'alias `*defaultConfig` pour réutiliser cette configuration de base, mais en écrasant la valeur de `api` pour notre environnement de production.

Il y a beaucoup d'autres fonctionnalités à découvrir en travaillant avec YAML, alors je vous encourage à explorer davantage si cela vous intéresse.

## Voir Aussi

Pour en savoir plus sur la syntaxe et les fonctionnalités de YAML, vous pouvez consulter la documentation officielle sur [yaml.org](https://yaml.org/). Si vous souhaitez explorer davantage l'utilisation de YAML dans Elm, vous pouvez également consulter le package [elm-exploration/yaml](https://package.elm-lang.org/packages/elm-exploration/yaml/latest/) sur le site des packages Elm.