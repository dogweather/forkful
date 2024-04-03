---
date: 2024-01-19
description: "Escrever um arquivo de texto permite salvar dados de forma persistente.\
  \ Programadores fazem isso para manter registros, configura\xE7\xF5es ou compartilhar\u2026"
lastmod: '2024-03-13T22:44:46.517906-06:00'
model: unknown
summary: Escrever um arquivo de texto permite salvar dados de forma persistente.
title: Escrevendo um arquivo de texto
weight: 24
---

## What & Why?
Escrever um arquivo de texto permite salvar dados de forma persistente. Programadores fazem isso para manter registros, configurações ou compartilhar informações entre sistemas.

## How to:
Elm é uma linguagem para criar aplicativos web e, por si só, não tem capacidade para escrever arquivos diretamente devido a restrições do navegador. No entanto, podemos gerar arquivos pra download. Veja como criar um arquivo de texto e disponibilizar pra download:

```Elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (href, download)
import Html.Events exposing (onClick)
import Json.Encode exposing (string)

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { content : String
  , uri : String
  }

init : Model
init =
  { content = "Texto de exemplo pra salvar no arquivo."
  , uri = ""
  }

type Msg
  = GenerateFile

update : Msg -> Model -> Model
update msg model =
  case msg of
    GenerateFile ->
      let
        encoded = string model.content
        uri = "data:text/plain;charset=utf-8," ++ encoded
      in
      { model | uri = uri }

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick GenerateFile ] [ text "Gerar arquivo de texto" ]
    , if model.uri == "" then
        text ""
      else
        a [ href model.uri, download "arquivo.txt" ] [ text "Baixar arquivo" ]
    ]
```
Após clicar no botão "Gerar arquivo de texto", um link de download é criado, permitindo que o usuário baixe o arquivo "arquivo.txt" com o conteúdo especificado.

## Deep Dive
Historicamente, Elm foca na criação de aplicativos seguros do lado do cliente, onde as operações de sistema de arquivos são limitadas por questões de segurança. Alternativas para trabalhar com arquivos envolvem o uso de Elm com JavaScript através de Ports, que pode ser usado para lidar com funcionalidades do sistema de arquivos do lado do servidor em Node.js, por exemplo. Detalhes de implementação levam em conta a codificação de URI e manipulação de dados para simular o processo de escrita de arquivo num ambiente de navegador.

## See Also
- [Elm Official Guide](https://guide.elm-lang.org/)
- [Elm File Saver Example](https://ellie-app.com/new)
- [Working with JavaScript in Elm using Ports](https://guide.elm-lang.org/interop/ports.html)
