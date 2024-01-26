---
title:                "Trabajando con JSON"
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Trabajar con JSON (JavaScript Object Notation) es manejar datos estructurados en un formato fácilmente intercambiable entre sistemas. Programadores lo usan porque es estándar en la web, ligero y comprensible por humanos y máquinas.

## Cómo Hacerlo:
```Elm
import Json.Decode as Decode

type alias User =
    { id : Int
    , username : String
    }

userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "id" Decode.int)
        (Decode.field "username" Decode.string)

jsonString : String
jsonString =
    """
    { "id": 42, "username": "elm_user" }
    """

decodeResult : Result String User
decodeResult =
    Decode.decodeString userDecoder jsonString

-- Salida esperada: Ok { id = 42, username = "elm_user" }
```

## Análisis Profundo:
El JSON surgió en los 2000 como respuesta a la necesidad de un estándar de intercambio de datos más eficiente que XML. En Elm, la manipulación de JSON se hace principalmente mediante decodificadores y codificadores. Alternativamente, herramientas como `elm-graphql` permiten trabajar con datos estructurados sin decodificadores. Implementar un decodificador en Elm es seguro y confiable gracias al sistema de tipos del lenguaje que ayuda a evitar errores en tiempo de ejecución.

## Ver También:
- Documentación sobre JSON en Elm: [Official Elm Guide - JSON](https://guide.elm-lang.org/effects/json.html)
- Paquete `elm/http` para hacer peticiones HTTP con Elm: [elm/http](https://package.elm-lang.org/packages/elm/http/latest/)
- Proyecto `elm-graphql`: [dillonkearns/elm-graphql](https://github.com/dillonkearns/elm-graphql)
