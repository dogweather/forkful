---
title:                "Trabalhando com csv"
html_title:           "Elm: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/working-with-csv.md"
---

{{< edit_this_page >}}

O que é e por que trabalhar com CSV:
CSV (Comma-Separated Values) é uma forma de armazenamento de dados em que os valores são separados por vírgulas. Programadores usam o formato CSV para organizar e compartilhar dados em uma estrutura simples e facilmente interpretável. É frequentemente utilizado em aplicativos que envolvem grandes conjuntos de dados como planilhas, bancos de dados e sistemas web.

Como fazer:
Para trabalhar com CSV em Elm, você pode usar a biblioteca "elm-csv" que facilita a leitura e escrita de arquivos CSV. Para ler um arquivo CSV, basta importar a biblioteca e usar a função "parse" com o caminho do arquivo como parâmetro. Por exemplo:
```
Elm CSV example

module Main exposing (..)

import Csv exposing (parse)

main : Program () Model Msg
main =
  let
    file = "./data.csv"
  in
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

type alias Model =
  List (List String)

type Msg
  = LoadCsv (List (List String))

init : ( Model, Cmd Msg )
init =
  ( [], Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LoadCsv content ->
      ( content, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  let
    rows = List.map (\row -> tr [] (List.map (\col -> td [] [ text col ]) row)) model
  in
    table [] rows
```

Aprofundando:
O formato CSV foi criado na década de 1970 para permitir a troca de informações entre diferentes programas. Ele é amplamente utilizado até hoje por sua simplicidade e facilidade de uso. Além disso, existem outras opções para armazenar dados, como JSON e XML, mas o CSV ainda é preferido quando se trata de compartilhar dados tabulares.

Veja também:
- Documentação oficial da biblioteca "elm-csv": https://package.elm-lang.org/packages/NoRedInk/elm-csv/latest/
- Exemplos de uso da biblioteca "elm-csv": https://github.com/NoRedInk/elm-csv/tree/master/examples