---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Elm: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que & Por que?

Enviar uma solicitação HTTP com autenticação básica é um processo comum entre programadores. Isso permite que o cliente se autentique com um servidor, fornecendo um nome de usuário e senha. Isso é feito para obter acesso a informações confidenciais ou realizar ações que requerem autenticação.

## Como fazer:

```Elm
{- Envio de solicitação HTTP básica com autenticação -}
import Http
import Json.Decode exposing (..)

{- Definindo as credenciais de autenticação -}
authCredentials =
    ( "username", "senha" )

{- Criando a solicitação HTTP -}
request =
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "Authorization" (Http.basicAuth authCredentials)
            ]
        , url = "https://exemplo.com/api/dados"
        , body = Http.emptyBody
        , expect = Http.expectJson decodeResponse
        , timeout = Nothing
        , tracker = Nothing
        }

{- Decodificando a resposta em JSON -}
type alias ResponseData =
    { dados : List String }

decodeResponse : Decoder ResponseData
decodeResponse =
    map ResponseData
        (field "dados" (list string))

{- Enviando a solicitação e obtendo a resposta -}
sendRequest : Cmd Msg
sendRequest =
    Http.send request

{- Executando a função sendRequest -}
init : () -> (Model, Cmd Msg)
init _ =
    ({ dados = [] }, sendRequest)

{- Atualizando o estado da aplicação com a resposta -}
type Msg
    = OnResponse (Result Http.Error ResponseData)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        OnResponse result ->
            case result of
                Ok response ->
                    ({model | dados = response.dados}, Cmd.none)

                Err _ ->
                    (model, Cmd.none)

{- Renderizando os dados obtidos da solicitação -}
view : Model -> Html Msg
view model =
    div []
        [ text (String.join ", " model.dados )
        ]

{- Alterando o main para incluir a função sendRequest -}
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }

{- Executando a aplicação -}
port app : Signal (Program Never () Msg)
port app =
    main
```

## Mergulho Profundo:

A autenticação básica via requisição HTTP foi introduzida no protocolo HTTP 1.0 para permitir que os clientes se autenticassem com servidores ao acessar conteúdo protegido. Existem outras formas de autenticação em que o cliente pode enviar suas credenciais, mas a autenticação básica é a mais simples e amplamente implementada.

## Veja Também:

[Documentação do Elm sobre módulos Http](https://package.elm-lang.org/packages/elm-lang/http/latest/)