---
title:                "Elm: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador en Elm, es muy probable que en algún momento necesites comunicar tu aplicación con algún servidor externo o API. En ese caso, necesitarás enviar solicitudes HTTP para obtener o enviar información. En esta publicación, aprenderá cómo enviar una solicitud HTTP en Elm y ampliará su conocimiento sobre este importante tema.

## Cómo hacerlo

Para enviar una solicitud HTTP en Elm, necesitarás usar la biblioteca nativa `Http`. Primero, deberás importar la biblioteca en tu módulo utilizando la siguiente línea de código:

```
import Http
```

Luego, puedes utilizar la función `send` para enviar una solicitud HTTP. Aquí hay un ejemplo de cómo enviar una solicitud GET a la API de Github para obtener información sobre un usuario específico:

```
Http.send getUserProfile
    { verb = "GET"
    , url = "https://api.github.com/users/elm"
    , body = Http.emptyBody
    , expect = Http.expectJson Decoders.userProfileDecoder
    }

type alias UserProfile =
    { name : String
    , email : Maybe String
    , followers : Int
    }

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    Json.Decode.map3 UserProfile
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.field "email" Json.Decode.string))
        (Json.Decode.field "followers" Json.Decode.int)
```

En este ejemplo, primero definimos un tipo de datos `UserProfile` que representa la información que esperamos recibir de la API de Github. Luego creamos un decodificador que se encargará de convertir la respuesta JSON en un valor de tipo `UserProfile`. Finalmente, utilizamos la función `expectJson` para indicarle a Elm que queremos procesar la respuesta utilizando nuestro decodificador.

## Profundizando

Enviar una solicitud HTTP en Elm es bastante simple, pero hay algunos conceptos importantes que debes entender antes de hacerlo. Por ejemplo, necesitarás conocer los diferentes tipos de solicitudes HTTP, como GET, POST, PUT y DELETE. También es importante saber cómo manejar errores y configurar encabezados en tus solicitudes.

Es posible que también necesites enviar datos junto con tu solicitud, por ejemplo al crear o actualizar un recurso en una API. Para hacer esto, puedes utilizar la función `Http.sendWithJsonBody` y proporcionar un objeto codificado en JSON como cuerpo de la solicitud.

## Ver también

- [Documentación oficial de la biblioteca Http en Elm](https://package.elm-lang.org/packages/elm/http/latest)
- [Tutorial de Elm sobre envío de solicitudes HTTP](https://guide.elm-lang.org/effects/http.html)
- [Ejemplo de solicitud POST en Elm](https://stackoverflow.com/questions/37342403/how-do-i-make-a-post-request-in-elm)
- [Ejemplo de solicitud DELETE en Elm](https://medium.com/@leotismu_elm/a-better-way-to-deal-with-delete-requests-in-elm-1e2d7242bc4a)