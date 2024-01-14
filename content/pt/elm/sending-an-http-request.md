---
title:                "Elm: Enviando uma requisição http"
simple_title:         "Enviando uma requisição http"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que enviar uma requisição HTTP em Elm?

Enviar requisições HTTP é uma tarefa comum em muitos projetos de programação. Em Elm, enviar uma requisição HTTP pode ser útil para obter dados de uma API externa ou para atualizar o estado de um aplicativo com dados do servidor. Neste artigo, discutiremos como enviar uma requisição HTTP em Elm e por que isso pode ser benéfico em seus projetos.

## Como enviar uma requisição HTTP em Elm

Para enviar uma requisição HTTP em Elm, primeiro precisamos importar o módulo `Http` em nosso código:

```Elm
import Http
```

Em seguida, declaramos uma função que realizará a requisição, especificando o método HTTP, o URL e os dados a serem enviados. Por exemplo, podemos criar uma função que envia uma requisição POST com alguns dados em formato JSON:

```Elm
sendRequest : Cmd Msg
sendRequest =
    Http.send
        { method = "POST"
        , url = "https://exemplo.com/api/users"
        , body = Http.jsonBody <| Json.Encode.object
            [ ( "name", Json.Encode.string "João" )
            , ( "email", Json.Encode.string "joao@example.com" )
            ]
        , expect = Http.expectJson Success (Json.Decode.succeed ())
        }
```

Em seguida, devemos lidar com a resposta na função `update` do nosso aplicativo:

```Elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Success response ->
            ( { model | response = response }, Cmd.none )

        Error error ->
            ( { model | error = Just error }, Cmd.none )

        -- outras mensagens e lógica do seu aplicativo
```

Uma vez que a requisição é enviada, o resultado pode ser tratado nas mensagens `Success` ou `Error`, dependendo se a requisição foi bem sucedida ou não. Podemos acessar a resposta da requisição através de `response` e tratar possíveis erros em `error`.

## Aprofundando no envio de requisições HTTP

Ao enviar uma requisição HTTP em Elm, existem alguns pontos importantes que devemos lembrar:

- Ao especificar o corpo da requisição, devemos usar funções do módulo `Http` como `jsonBody` ou `stringBody` para garantir a correta serialização dos dados.
- Podemos usar a função `expectString` para lidar com dados em formato de texto na resposta da requisição.
- O módulo `Http` fornece diversas funções de alta ordem, como `expectJson`, `expectString` e `expectBytes`, que nos permitem especificar como lidar com a resposta da requisição de acordo com seu tipo de conteúdo.

Tenha em mente que, ao enviar uma requisição HTTP em Elm, é importante sempre tratar possíveis erros e garantir que os dados sejam enviados e recebidos corretamente.

## Veja também

- Documentação do módulo `Http` em [Elm Packages](https://package.elm-lang.org/packages/elm/http/latest/Http)
- Tutorial sobre como [fazer requisições HTTP em Elm](https://www.youtube.com/watch?v=r3JYjRt_VTg) (em inglês)
- Exemplo de [aplicativo Elm](https://github.com/rtfeldman/elm-spa-example) que utiliza requisições HTTP (em inglês)