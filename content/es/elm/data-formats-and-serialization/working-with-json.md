---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:54.231295-07:00
description: "Trabajar con JSON en Elm consiste en decodificar datos JSON a tipos\
  \ de Elm y codificar valores de Elm de vuelta a JSON. Este proceso es crucial para\
  \ que\u2026"
lastmod: '2024-03-13T22:44:59.014991-06:00'
model: gpt-4-0125-preview
summary: Trabajar con JSON en Elm consiste en decodificar datos JSON a tipos de Elm
  y codificar valores de Elm de vuelta a JSON.
title: Trabajando con JSON
weight: 38
---

## ¿Qué & Por qué?
Trabajar con JSON en Elm consiste en decodificar datos JSON a tipos de Elm y codificar valores de Elm de vuelta a JSON. Este proceso es crucial para que las aplicaciones web interactúen con APIs y fuentes de datos externas, permitiendo un intercambio de datos fluido entre el cliente (Elm) y servidores u otros servicios.

## Cómo hacerlo:

Elm trata el manejo de JSON con explicitud y seguridad, utilizando principalmente los módulos `Json.Decode` y `Json.Encode`. Para empezar a trabajar con JSON, primero necesitas definir un decodificador para tu tipo de datos. Supongamos que estamos tratando con un objeto de perfil de usuario simple.

Primero, define tu tipo en Elm:

```elm
type alias UserProfile = 
    { id : Int
    , name : String
    , email : String
    }
```

### Decodificando JSON a Elm

Para decodificar una cadena JSON al tipo `UserProfile`, crea un decodificador:

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map3 UserProfile
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

Para decodificar un objeto JSON:

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decoded : Result String UserProfile
decoded =
    decodeString userProfileDecoder jsonString

{- Ejemplo de Salida:
Result.Ok { id = 1, name = "John Doe", email = "john@example.com" }
-}
```

### Codificando Elm a JSON

Para codificar un valor Elm de vuelta a JSON, utiliza el módulo `Json.Encode`.

```elm
import Json.Encode exposing (object, int, string)

encodeUserProfile : UserProfile -> String
encodeUserProfile userProfile =
    object
        [ ("id", int userProfile.id)
        , ("name", string userProfile.name)
        , ("email", string userProfile.email)
        ]
        |> Json.Encode.encode 0

{-
Uso:
encodeUserProfile { id = 1, name = "John Doe", email = "john@example.com" }

Ejemplo de Salida:
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### Bibliotecas de Terceros

Paquetes de Elm como `elm-json-decode-pipeline` pueden simplificar la creación de decodificadores utilizando un estilo de pipeline, lo cual es especialmente útil para decodificar objetos complejos.

Primero, agrega la biblioteca a tu proyecto:

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

Luego, puedes simplificar la definición del decodificador de la siguiente manera:

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- Utiliza este decodificador como antes con decodeString para decodificar cadenas JSON. -}
```

Este enfoque simplifica el decodificador, haciendo que el código sea más limpio y mantenible, especialmente a medida que las estructuras de datos se vuelven más complejas.
