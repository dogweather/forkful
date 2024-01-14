---
title:                "Elm: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Porquê

Muitas vezes, quando construímos aplicações web, é necessário interagir com servidores externos para obter ou enviar informações. Para garantir que essas interações sejam seguras, é comum usar autenticação básica com solicitações HTTP. Neste artigo, vamos explorar como enviar uma solicitação HTTP com autenticação básica usando Elm.

## Como Fazer

Para enviar uma solicitação HTTP com autenticação básica em Elm, primeiro precisamos instalar o pacote `elm/http` através do gerenciador de pacotes do Elm. Em seguida, usamos a função `send` do módulo `Http` para fazer a solicitação. Aqui está um exemplo de como ficaria o código:

```Elm
import Http
import Basics

-- URL do servidor externo
url = "https://api.com/solicitacao"

-- Header com autenticação básica
authHeader = ("Authorization", "Basic " ++ Basics.toBase64 "username:password")

-- Função para lidar com o resultado da solicitação
handleResult : Http.Result String -> Msg
handleResult result =
    case result of
        Http.Ok response ->
            -- Lógica para manipular a resposta do servidor
        Http.Err error ->
            -- Lógica para lidar com o erro

-- Função para enviar a solicitação com autenticação básica
sendRequest : Cmd Msg
sendRequest =
    Http.post
        { url = url
        , body = Http.emptyBody
        , headers = [authHeader]
        , expect = Http.expectString handleResult
        }

-- Usamos essa função no `update` para enviar a solicitação quando necessário
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MakeRequest ->
            ( model, sendRequest )

```

O código acima é um exemplo simples de como enviar uma solicitação com autenticação básica em Elm. A função `toBase64` do módulo `Basics` é usada para codificar nosso nome de usuário e senha em Base64, que é o formato necessário para a autenticação básica.

## Explorando Mais

Há várias coisas a serem consideradas ao enviar uma solicitação HTTP com autenticação básica. Por exemplo, é importante lidar com possíveis erros e validar a resposta do servidor antes de usá-la. Além disso, é possível enviar dados no corpo da solicitação e usar diferentes métodos HTTP, como `get`, `post` e `put`. Recomenda-se ler a documentação completa do pacote `elm/http` para entender todas as possibilidades.

## Veja Também

- Documentação do pacote `elm/http`: https://package.elm-lang.org/packages/elm/http/latest/
- Tutorial sobre autenticação básica em Elm: https://medium.com/@ryannhg/basic-authentication-with-elm-408c0f379763