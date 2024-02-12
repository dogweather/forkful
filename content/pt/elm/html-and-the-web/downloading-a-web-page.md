---
title:                "Baixando uma página da web"
aliases:
- /pt/elm/downloading-a-web-page/
date:                  2024-01-20T17:44:09.952664-07:00
model:                 gpt-4-1106-preview
simple_title:         "Baixando uma página da web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Baixar uma página web significa pegar o conteúdo dela, geralmente em HTML, através da internet. Programadores fazem isso para processar essa informação, extrair dados ou até mesmo para testar a disponibilidade e o tempo de resposta das páginas.

## Como fazer:
Elm torna a tarefa de baixar uma página web uma jornada tranquila, principalmente com o pacote `Http`. Vamos ver como fazer isso na prática.

```Elm
import Browser
import Http
import Html exposing (Html, text)

type alias Model =
    { content : String }

type Msg
    = Fetch
    | FetchComplete (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch ->
            ( model, Http.get { url = "https://exemplo.com", expect = Http.expectString FetchComplete } )

        FetchComplete (Ok data) ->
            ( { model | content = data }, Cmd.none )

        FetchComplete (Err _) ->
            ( { model | content = "Não foi possível baixar o conteúdo." }, Cmd.none )

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( { content = "" }, Cmd.none )
        , update = update
        , view = \model -> text model.content
        , subscriptions = \_ -> Sub.none
        }
```

A saída será o conteúdo HTML da página "https://exemplo.com" se o download for bem-sucedido ou uma mensagem de erro se algo der errado.

## Aprofundamento
Historicamente, baixar páginas web era uma tarefa mais complicada em Elm devido à sua natureza fortemente tipada e ao foco em evitar efeitos colaterais. Entretanto, com o aparecimento dos pacotes como `Http`, isso se tornou muito mais direto. Existem alternativas, como usar `WebSockets` para um fluxo de dados contínuo ou usar APIs GraphQL, mas para uma simples requisição HTTP, `Http.get` é a sua escolha.

Quanto aos detalhes de implementação, Elm trata downloads de página web de maneira segura, evitando os problemas comuns de segurança como Cross-Site Scripting (XSS) por forçar o uso de tipos para representar HTML e Eventos. Isso assegura que você trabalhe com dados seguros bem definidos em vez de strings cruas suscetíveis a ataques.

## Veja também
- Documentação oficial de Elm para o pacote `Http`: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Um guia prático de Elm: [https://guide.elm-lang.org/](https://guide.elm-lang.org/)
