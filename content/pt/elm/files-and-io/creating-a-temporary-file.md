---
date: 2024-01-20 17:40:05.770021-07:00
description: "How to: Como Elm \xE9 uma linguagem para construir interfaces de usu\xE1\
  rio no navegador, n\xE3o tem acesso direto ao sistema de arquivos do dispositivo.\
  \ Portanto,\u2026"
lastmod: '2024-03-13T22:44:46.518770-06:00'
model: gpt-4-1106-preview
summary: "Como Elm \xE9 uma linguagem para construir interfaces de usu\xE1rio no navegador,\
  \ n\xE3o tem acesso direto ao sistema de arquivos do dispositivo."
title: "Criando um arquivo tempor\xE1rio"
weight: 21
---

## How to:
Como Elm é uma linguagem para construir interfaces de usuário no navegador, não tem acesso direto ao sistema de arquivos do dispositivo. Portanto, em Elm, você não pode criar um arquivo temporário no sentido tradicional. Em vez disso, vamos focar em como você pode gerar dados para downloads temporários (que podem ser considerados arquivos temporários no contexto de aplicações web).

```Elm
import Browser
import Html exposing (Html, button, text)
import Html.Events exposing (onClick)

type Msg = Download

view : Html Msg
view =
    button [ onClick Download ] [ text "Download Temp File" ]

subscriptions : Msg -> Sub Msg
subscriptions _ =
    Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Download ->
            ( model
            , Browser.Navigation.load (dataUrl "text/plain" "Hello, this is temporary content.")
            )

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> (model, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

dataUrl : String -> String -> String
dataUrl mimeType content =
    "data:" ++ mimeType ++ ";base64," ++ (toBase64 content)

toBase64 : String -> String
toBase64 content =
    -- Elm doesn't have a built-in Base64 encoder, assuming here a custom implementation or a third-party package.
    -- This is just a placeholder for the sake of example.
    "Base64EncodedContent"

-- Don't forget to replace "-- Elm doesn't have ..." part with actual Base64 encoding!
```

Este exemplo mostra um botão que, quando clicado, dispara o download de um "arquivo" temporário.

## Deep Dive
Elm foi projetado para aplicações web seguras e mantidas. Devido a isso, não oferece acesso direto ao sistema de arquivos para evitar problemas de segurança. Alternativas para criação de arquivos temporários em uma aplicação Elm passam por usar interop com JavaScript através de ports. Em JavaScript, a API `File` e `Blob` pode ser usada para criar e gerenciar arquivos temporários e dinâmicos. Em ambientes fora do navegador, como servidores ou scripts locais, você poderia usar outros idiomas como Python ou JavaScript com Node.js para manipular arquivos temporários diretamente.

## See Also
- Elm Ports: https://guide.elm-lang.org/interop/ports.html
- JavaScript `Blob` object: https://developer.mozilla.org/en-US/docs/Web/API/Blob
- Creating and downloading files in JavaScript: https://developer.mozilla.org/en-US/docs/Web/API/Blob#creating_and_downloading_files
