---
title:                "Elm: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Pourquoi

JSON (JavaScript Object Notation) est un format de données très répandu dans le développement web et mobile. Il permet de structurer des données de manière simple et lisible pour les humains et les machines. En travaillant avec JSON en Elm, vous pourrez charger et manipuler facilement des données dynamiques dans vos applications.

## Comment faire

Pour travailler avec JSON en Elm, vous avez besoin du module `Json.Decode` pour decoder des données JSON en une structure Elm, et du module `Json.Encode` pour encoder des données Elm en JSON. Voici un exemple de code pour decoder et encoder une liste de personnes:

```Elm
import Json.Decode exposing (..)
import Json.Encode

type alias Person =
  { name : String
  , age : Int
  }

peopleDecoder : Decoder (List Person)
peopleDecoder =
  list
    <| map2 Person
        (field "name" string)
        (field "age" int)

peopleEncoder : List Person -> Value
peopleEncoder people =
  List.map
    (\person -> object
        [ ( "name", string person.name )
        , ( "age", int person.age )
        ]
    )
    people
    |> list

jsonString : String
jsonString =
  """
  [
    { "name": "Alice", "age": 22 },
    { "name": "Bob", "age": 25 }
  ]
  """

people : Result String (List Person)
people =
  decodeString peopleDecoder jsonString

JsonDecode.decodeString : JsonDecode.Decoder (List Person) JsonDecode.Result (List Person)

peopleOk =
  case people of
    Ok list ->
      list

    Err message ->
      debug message

JsonDecodeResult a

peopleToWrite : List Person
peopleToWrite =
  [ { name = "Clémence", age = 21 }
  , { name = "Damien", age = 28 }
  ] 

JsonEncoderValue

peopleInJson : Value
peopleInJson =
  encode 0 (peopleEncoder peopleToWrite)

```

Le décodeur `peopleDecoder` utilise les fonctions `field` pour extraire les données de chaque entrée et les former dans la structure `Person`. Le codeur `peopleEncoder` utilise la fonction `object` pour créer un objet JSON contenant les données de chaque personne.

## Profondeur

En utilisant les fonctions de décodage et d'encodage du module `Json` d'Elm, vous pouvez manipuler des données JSON de manière plus avancée. Vous pouvez également utiliser le type `Value` pour représenter une valeur JSON arbitraire, ainsi que les fonctions `encode` et `decode` pour traiter des valeurs `Value`.

Si vous souhaitez en savoir plus sur la manipulation de JSON en Elm, vous pouvez consulter la documentation officielle du module `Json` ainsi que le guide détaillé sur la gestion de données dynamiques.

## Voir aussi

- [Documentation officielle du module `Json`](https://package.elm-lang.org/packages/elm/json/latest/)
- [Guide sur la gestion de données dynamiques en Elm](https://guide.elm-lang.org/effects/json.html)