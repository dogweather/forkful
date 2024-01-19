---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Generowanie losowych liczb to proces tworzenia ciągu liczb, które nie mają wzajemnej korelacji. Programiści korzystają z tego do tworzenia różnorodnych sytuacji, takich jak symulacje, gry, testy równoległe czy szyfrowanie danych.

## Jak to zrobić:
Generowanie liczby losowej w Elm jest proste. Poniżej znajdziesz przykładowy kod:

```Elm
import Random

generateRandomNumber : Int -> Int -> Cmd Msg
generateRandomNumber min max =
    Random.generate NewNumber (Random.int min max)

type Msg
    = NewNumber Int

-- Plik Compile.elm
module Main exposing (..)

import Browser
import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Random

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model =
    { randomNumber : Int }

init : Model
init =
    { randomNumber = 0 }

-- UPDATE

type Msg
    = Generate

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate (Random.int 0 100) )

```
Ten kod generuje losową liczbę z zakresu od 0 do 100.

## Deep Dive
Generowanie liczb losowych ma głębokie korzenie w informatyce, a jednym z pierwszych sposobów były metody sprzętowe. Z czasem odpowiednio programowano komputery do generowania tych losowości. Fellowship of the Ring (FOTR) to jedna z najstarszych metod generacji liczb losowych, używana jeszcze przez komputer Manchester Mark I.

Na przestrzeni lat, różne implementacje i metody generowania liczb losowych były rozwijane. Na przykład, języki programowania takie jak Python i JavaScript mają własne wbudowane funkcje do generowania liczb losowych.

W kontekście Elma, Random.generate jest używany do generowania losowego wyniku. Jest to jednak efekt, który musi być obsłużony w modelu update.

## Zobacz również
Link do dokumentacji Elm na temat modułu losowości: https://package.elm-lang.org/packages/elm/random/latest/

Inne metody generowania liczb losowych, takie jak LFSR (Linear Feedback Shift Register): https://en.wikipedia.org/wiki/Linear-feedback_shift_register

Historia generowania liczb losowych: https://pl.wikipedia.org/wiki/Generator_liczb_losowych

Przykłady użycia liczb losowych w praktyce: https://levelup.gitconnected.com/real-world-uses-of-random-numbers-8e404ba1c94e