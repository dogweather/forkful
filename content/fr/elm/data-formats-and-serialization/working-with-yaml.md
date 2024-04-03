---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:22.165548-07:00
description: "Comment faire : Pour g\xE9rer YAML dans Elm, vous devez g\xE9n\xE9ralement\
  \ convertir YAML en JSON en dehors d'Elm, puis utiliser la fonctionnalit\xE9 de\
  \ d\xE9codeur JSON\u2026"
lastmod: '2024-03-13T22:44:57.711972-06:00'
model: gpt-4-0125-preview
summary: "Pour g\xE9rer YAML dans Elm, vous devez g\xE9n\xE9ralement convertir YAML\
  \ en JSON en dehors d'Elm, puis utiliser la fonctionnalit\xE9 de d\xE9codeur JSON\
  \ int\xE9gr\xE9e \xE0 Elm pour travailler avec les donn\xE9es."
title: Travailler avec YAML
weight: 41
---

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
