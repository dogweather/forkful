---
date: 2024-01-20 17:44:06.008784-07:00
description: "How to: Para descargar una web en Elm, usamos el m\xF3dulo `Http`. Aqu\xED\
  \ hay un ejemplo simple que muestra c\xF3mo hacer una petici\xF3n GET."
lastmod: '2024-03-13T22:44:58.978767-06:00'
model: gpt-4-1106-preview
summary: "Para descargar una web en Elm, usamos el m\xF3dulo `Http`."
title: "Descargando una p\xE1gina web"
weight: 42
---

## How to:
Para descargar una web en Elm, usamos el módulo `Http`. Aquí hay un ejemplo simple que muestra cómo hacer una petición GET:

```Elm
import Http
import Json.Decode exposing (string)

type Msg
    = GotText (Result Http.Error String)

getText : Cmd Msg
getText =
    Http.get
        { url = "https://example.com"
        , expect = Http.expectString GotText
        }

-- Código para la inicialización y suscripciones aquí

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotText (Ok data) ->
            ( { model | content = data }, Cmd.none )

        GotText (Err _) ->
            ( model, Cmd.none )

-- Resto del modelo, view, y funciones auxiliares
```

Si ejecutas este código en una aplicación Elm, harás una petición a `example.com` y manejarás el resultado.

## Deep Dive
Elm nació para hacer aplicaciones web confiables. Con Elm, evitas errores en tiempo de ejecución usando un sistema de tipos fuerte.

Para bajar páginas web, Elm proporciona el módulo `Http`. Es funcional y usa `Commands` para efectuar operaciones de lado, como peticiones HTTP, que no encajan en Elm's mundo puramente funcional.

Hay alternativas como `XmlHttpRequest` en JavaScript, pero Elm maneja efectos de lado de una manera más segura y predecible.

Detalles de implementación: Elm usa comandos (`Cmd`) para peticiones asíncronas, que retornan mensajes (`Msg`) que tu programa maneja. Necesitas decodificadores (`Decoder`) para manejar el contenido de la respuesta ya que Elm es fuertemente tipado.

## See Also
- [Elm Guide – HTTP](https://guide.elm-lang.org/effects/http.html)
- [Elm Http Package](http://package.elm-lang.org/packages/elm/http/latest)
- [Json.Decode](https://package.elm-lang.org/packages/elm/json/latest/Json-Decode)
