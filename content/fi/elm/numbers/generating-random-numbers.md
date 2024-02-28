---
title:                "Sattumanvaraisten numeroiden generointi"
date:                  2024-02-27T22:50:15.929283-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-02-27, dogweather, edited and tested
  - 2024-02-27, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Mitä & Miksi?
Satunnaislukujen generointi Elm-kielessä vaatii `Random`-moduulin käyttöä pseudo-satunnaislukujen tuottamiseen, jotka ovat hyödyllisiä monenlaisissa tehtävissä kuten peleissä, simulaatioissa ja jopa algoritmeissä, jotka vaativat stokastisia prosesseja. Tämä ominaisuus mahdollistaa kehittäjille lisätä arvaamattomuutta ja monimuotoisuutta sovelluksiinsa, parantaen käyttäjäkokemusta ja toiminnallisuutta.

## Kuinka:
Elmin puhtaasti funktionaalinen luonne tarkoittaa, että et voi generoida satunnaislukuja suoraan kuten imperatiivisissa kielissä. Sen sijaan käytät `Random`-moduulia yhdessä komentojen kanssa. Tässä on perusesimerkki, joka generoi satunnaisen kokonaisluvun väliltä 1 ja 100.

Asenna ensin `Random`-moduuli komennolla `elm install elm/random`. Tuo se sitten Elm-tiedostoosi, yhdessä tarvittavien HTML- ja tapahtumamoduulien kanssa, näin:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

Jotta tämä on itsenäinen esimerkki, voit lisätä tämän rungon:
```elm
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

Seuraavaksi määrittele **komento** satunnaisluvun generoimiseksi. Tämä sisältää `Msg`-tyypin määrittelyn satunnaisluvun käsittelyä varten, kun se on generoitu, `Model`in sen tallentamiseen ja päivitysfunktion kaiken yhdistämiseksi.
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom number ->
            ( { model | randomNumber = number }, Cmd.none )
```

Satunnaisluvun generoimisen laukaisemiseksi lähettäisit `Generate`-viestin, esimerkiksi napin kautta näkymässäsi:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Satunnaisluku: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Generoi" ]
        ]
```

Kun klikkaat "Generoi"-nappia, näytölle tulee satunnainen luku väliltä 1 ja 100.

Tämä yksinkertainen lähestymistapa voidaan sovittaa ja laajentaa, hyödyntämällä muita `Random`-moduulin toimintoja satunnaisten liukulukujen, listojen tai jopa monimutkaisten tietorakenteiden tuottamiseen perustuen mukautettuihin tyyppeihin, tarjoten laajan leikkipaikan arvaamattomuuden lisäämiseen Elm-sovelluksiisi.

Elm-opas käsittelee tätä paljon yksityiskohtaisemmin. Siinä on myös [esimerkki kuusisivuisen nopan heittämisestä](https://guide.elm-lang.org/effects/random).
