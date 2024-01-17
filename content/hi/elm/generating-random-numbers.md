---
title:                "संयुक्त बाधरण द्वारा संख्याओं का उत्पादन"
html_title:           "Elm: संयुक्त बाधरण द्वारा संख्याओं का उत्पादन"
simple_title:         "संयुक्त बाधरण द्वारा संख्याओं का उत्पादन"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
Generating random numbers एक programming technique है जिसमें हम अनुक्रमिक तारणाएं बनाते हैं। हमारे प्रोग्राम विभिन्न सांख्यिकीय या रंगबिरंगे डेटा को analysis और simulation करने के लिए random numbers की आवश्यकता होती है।

## कैसे करें:
```Elm
import Random

-- random numbers between 0 and 100
randomNumberList : List Int
randomNumberList =
    Random.list 10 (Random.int 0 100)

main : Program () Model Msg
main =
    program
        { init = (Model "" randomNumberList, Cmd.none)
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
        
view : Model -> Html Msg
view model =
    div [] [
        h1 [ text "Random Numbers in Elm" ],
        ul [] (List.map (\num -> li [ text <| String.fromInt num ]) model.randomNumberList)
    ]
    
type Model
    = Model String (List Int)
    
type Msg
    = NoOp

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
```

## गहराई में जाएं:
इतिहासिक पृष्ठभूमि के पार जाकर, random numbers को सांख्यिकी में बहुत समय से प्रयोग किया जाता है। एल्गोरिथमों और तकनीकियों में पिछले सौ सालों में बहुत उन्नति हुई है और आज वे प्रकाशित हो चुके हैं। अलग-अलग programming languages में भी random numbers को उत्पन्न करने के लिए कई तकनीक उपलब्ध हैं, जैसे pseudo-random और true random algorithms, seed values और बहुत कुछ।

## इससे संबंधित देखें:
[Elm Random Library](https://package.elm-lang.org/packages/elm/random/latest), [Understanding Randomness and Random Numbers](https://www.lifewire.com/understanding-randomness-and-random-numbers-958201), [Random Number Generation in Different Programming Languages](https://hackernoon.com/random-number-generation-in-different-programming-languages-example-python-8e90c160d92e)