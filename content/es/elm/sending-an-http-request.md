---
title:                "Enviando una solicitud http"
html_title:           "Elm: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

El envío de una solicitud HTTP es una forma en que los desarrolladores pueden interactuar con recursos en la web, como páginas web o servicios en línea. Los programadores necesitan enviar solicitudes HTTP para acceder y manipular datos en línea.

## Como hacer:

Los siguientes son ejemplos de cómo realizar una solicitud HTTP en Elm:

```Elm
import Http
import Json.Decode exposing (int, string, list)

type Msg
  = GetUsers
  | UsersResult (Result Http.Error (List User))

msgDecoder : Decoder Msg
msgDecoder =
  Json.Decode.where
    "action" (Json.Decode.succeed GetUsers)

type alias User =
  { name: String
  , age: Int
  }

userDecoder : Decoder User
userDecoder =
  Json.Decode.map2 User
    (Json.Decode.field "name" string)
    (Json.Decode.field "age" int)
    
type alias Error =
  { status: Int
  , message: String
  }

httpGetUsers : Cmd Msg
httpGetUsers =
  Http.get
    { url = "https://example.com/users"
    , expect = Http.expectJson UsersResult (Json.Decode.list userDecoder)
    }

main =
  App.program
    { init = (model, httpGetUsers)
    , update = update
    , view = view
    , subscriptions = (\msg -> ())
    }

```

La salida de este código sería una lista de usuarios en formato JSON.

## Inmersión profunda:

El envío de solicitudes HTTP ha sido una parte fundamental del desarrollo web durante años, y hay muchas alternativas disponibles, como JQuery o Fetch API. En Elm, el módulo Http proporciona una forma segura y fácil de realizar solicitudes HTTP, utilizando efectos de comando y decodificadores para manejar posibles errores.

## Ver también:

Para obtener más información sobre el envío de solicitudes HTTP en Elm, puede consultar la documentación oficial de Elm: https://guide.elm-lang.org/effects/http.html. Encontrará información detallada sobre cómo usar el módulo Http y las diferentes opciones disponibles.