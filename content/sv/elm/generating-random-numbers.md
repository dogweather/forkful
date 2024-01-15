---
title:                "Skapa slumpmässiga tal"
html_title:           "Elm: Skapa slumpmässiga tal"
simple_title:         "Skapa slumpmässiga tal"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Varför

Att generera slumpmässiga nummer är en vanlig uppgift inom många programmeringsprojekt. Det kan ha många olika användningsområden, från spelutveckling till datavisualisering. Att kunna producera slumpmässiga nummer är en viktig programmeringsfärdighet som kan hjälpa till att göra programmen mer intressanta och dynamiska.

## Så här

För att generera slumpmässiga nummer i Elm, används funktionen `Random.generate`. Den tar två argument: en generatorfunktion och en signal för att kommunicera med Elm Runtime-systemet. Detta är ett exempel på hur man kan använda funktionen för att generera en lista med 10 slumpmässiga heltal mellan 1 och 100:

```Elm
module Main exposing (..)

import Html exposing (text)
import Random exposing (Generator, int, step, generate)

main =
  Html.beginnerProgram
    { model = 0
    , view = view
    , update = update
    }

type Msg
  = GenerateRandList

view model =
  text (String.fromInt model)

update msg model =
  case msg of
    GenerateRandList ->
      (model, Random.generate (chosenList 10 1 100) RandList)

chosenList : Int -> Int -> Int -> Generator (List Int)
chosenList count min max =
  Random.list count (int min max)

type RandList = RandList (List Int)

randListSignal : Signal RandList
randListSignal =
  Signal.map RandList (Random.step chosenList)

```

### Output:
![Output Image](https://i.imgur.com/TEwn9F6.png)

## Djupdykning

Elm använder sig av en pseudo-slumpmässig algoritm för att generera de slumpmässiga numren. Detta kan låta som en komplicerad process, men det enda vi behöver veta är att det är tillräckligt för de flesta användningsområden.

För de som är intresserade av att lära sig mer om hur slumpmässiga tal genereras och vilka algoritmer som används, finns det många resurser tillgängliga på nätet. En stor fördel med att lära sig hur detta fungerar är att man kan skapa anpassade generatorfunktioner för att producera specifika typer av slumpmässiga nummer, vilket kan vara användbart i vissa scenarier.

## Se även

- [Elm Random package](https://package.elm-lang.org/packages/elm/random/latest/)
- [Random Number Generator Algorithm](https://en.wikipedia.org/wiki/Random_number_generation)
- [Generating Random Numbers in Elm](https://www.youtube.com/watch?v=1Gt9eV0dqIY)