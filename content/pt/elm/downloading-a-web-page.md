---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

##O Que & Porquê?

Baixar uma página da web é o processo de copiar e salvar o conteúdo de uma página da web em um dispositivo local. Programadores fazem isso por várias razões, como para criar backups, analisar páginas da web ou trabalhar offline.

##Como Fazer:

Infelizmente, Elm, por ser uma linguagem funcional voltada para a arquitetura de front-end, não possui módulos incorporados para baixar páginas da web diretamente. No entanto, você pode usar requisições HTTP para obter o conteúdo de uma página da web. 

####Instalando o pacote:

```Elm
elm install elm/http
```

####Exemplo de código:

```Elm
module Main exposing (..)

import Browser
import Html exposing (..)
import Http

type alias Model =
    { content : String }

init : Model
init =
    { content = "" }

type Msg
    = LoadContent
    | ContentLoaded (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadContent ->
            ( model
            , Http.get
                { url = "http://example.com"
                , expect = Http.expectString ContentLoaded
                }
            )

        ContentLoaded (Ok content) ->
            ( { model | content = content }, Cmd.none )

        ContentLoaded (Err _) ->
            ( model, Cmd.none )

main =
    Browser.sandbox { init = init, update = update, view = \_ -> div [] [] }
```

Aqui, quando o comando `LoadContent` é executado, uma requisição HTTP é feita para http://example.com. O conteúdo da página da web é então atualizado em nossa model.

##Fundo Histórico

O Elm foi criado em 2012 por Evan Czaplicki, então um estudante, despertou a atenção da comunidade de desenvolvimento por seus recursos para aplicativos de página única robustos e confiáveis. E embora o Elm seja principalmente uma linguagem de front-end, ela pode receber dados por meio de requisições HTTP, que é como você pode "baixar" o conteúdo de uma página da web no idioma.

##Alternativas

Se você estiver procurando uma solução mais robusta que se aproxime do que geralmente significa baixar um site, talvez seja melhor usar outra linguagem que permita o scraping de sites, como Python ou Node.js.

##Detalhes da Implementação

Quanto aos detalhes da implementação, a função Http.get é usada para fazer a requisição HTTP. A url do site deve ser incluída como um dos parâmetros. O segundo parâmetro é "esperar", que define o tipo de resposta que esperamos receber do site. No caso acima, estamos esperando uma string.

##Veja Também

[Elm HTTP](https://package.elm-lang.org/packages/elm/http/latest/)

[Elm Guide](https://guide.elm-lang.org/)

[Elm Syntax](https://elm-lang.org/docs/syntax)