---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:22.165548-07:00
description: "Elm n'int\xE8gre pas de support natif pour YAML, un format de s\xE9\
  rialisation de donn\xE9es souvent utilis\xE9 pour les fichiers de configuration\
  \ ou le partage de\u2026"
lastmod: '2024-03-13T22:44:57.711972-06:00'
model: gpt-4-0125-preview
summary: "Elm n'int\xE8gre pas de support natif pour YAML, un format de s\xE9rialisation\
  \ de donn\xE9es souvent utilis\xE9 pour les fichiers de configuration ou le partage\
  \ de donn\xE9es, en raison de son fort accent sur la s\xE9curit\xE9 de type et les\
  \ r\xE9sultats pr\xE9visibles."
title: Travailler avec YAML
weight: 41
---

## Quoi & Pourquoi ?
Elm n'intègre pas de support natif pour YAML, un format de sérialisation de données souvent utilisé pour les fichiers de configuration ou le partage de données, en raison de son fort accent sur la sécurité de type et les résultats prévisibles. Cependant, les programmeurs rencontrent fréquemment YAML lorsqu'ils gèrent des API ou des configurations en développement web, nécessitant des méthodes fiables pour analyser les données YAML dans l'écosystème strictement typé d'Elm pour une intégration et une manipulation transparentes.

## Comment faire :
Pour gérer YAML dans Elm, vous devez généralement convertir YAML en JSON en dehors d'Elm, puis utiliser la fonctionnalité de décodeur JSON intégrée à Elm pour travailler avec les données. Bien que cette approche nécessite une étape de conversion supplémentaire, elle tire parti du système de types stricts d'Elm pour garantir l'intégrité des données. Les outils populaires pour la conversion de YAML en JSON incluent les convertisseurs en ligne ou les services backend. Une fois que vous avez JSON, vous pouvez utiliser le module `Json.Decode` d'Elm pour travailler avec les données.

Premièrement, en supposant que vous avez les données YAML suivantes :

```yaml
person:
  name: Jane Doe
  age: 30
```

Convertissez-les en format JSON :

```json
{
  "person": {
    "name": "Jane Doe",
    "age": 30
  }
}
```

Ensuite, définissez votre modèle et décodeur Elm :

```elm
module Main exposing (..)

import Html exposing (text)
import Json.Decode as Decode

type alias Person =
    { name : String
    , age : Int
    }

personDecoder : Decode.Decoder Person
personDecoder =
    Decode.map2 Person
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

```

Pour utiliser ce décodeur pour convertir JSON en un type Elm :

```elm
import Json.Decode as Decode

jsonString = 
    """
    {
      "person": {
        "name": "Jane Doe",
        "age": 30
      }
    }
    """

decodeResult = Decode.decodeString (Decode.field "person" personDecoder) jsonString

main =
    case decodeResult of
        Ok person ->
            Html.text ("Bonjour, " ++ person.name ++ "!")

        Err _ ->
            Html.text "Une erreur s'est produite lors du décodage."
```

Sortie (rendue dans une application Elm) :
```
Bonjour, Jane Doe !
```

Cette approche garantit que vous pouvez travailler avec des données YAML dans Elm en utilisant JSON comme format intermédiaire, tirant parti du système de types robuste d'Elm et des capacités de décodage JSON pour manipuler de manière sûre et efficace des données externes.
