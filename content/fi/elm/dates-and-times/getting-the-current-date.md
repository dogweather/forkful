---
title:                "Nykyisen päivämäärän hankkiminen"
aliases: - /fi/elm/getting-the-current-date.md
date:                  2024-01-20T15:14:14.752219-07:00
simple_title:         "Nykyisen päivämäärän hankkiminen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? ("Mitä & Miksi?")
Saada nykyinen päivämäärä tarkoittaa kellonaikaan sidotun päivämäärätiedon hyödyntämistä. Käytämme tätä toiminnallisuutta esimerkiksi lokeissa, aikaleimoissa tai käyttäjän toiminnan ajastamisessa.

## How to: ("Kuinka tehdään:")
Elmissä työskennellessä voit käyttää `Time` moduulia nykyisen päivämäärän saamiseksi. Tässä pikainen esimerkki:

```Elm
import Browser
import Html exposing (Html, text)
import Task
import Time exposing (Posix)

main =
    Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

type alias Model = Posix

init : () -> (Model, Cmd Msg)
init _ =
    (Time.millisToPosix 0, Task.perform NewTime Time.now)

type Msg
    = NewTime Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewTime posix ->
            (posix, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Html Msg
view model =
    text (String.fromInt (Time.toMillis model))

type alias Flags = ()

```

Ajamalla yllä olevan koodin saat tulostettua selaimen konsoliin nykyisen ajan millisekunteina Unix Epochista (1. tammikuuta 1970) lähtien.

## Deep Dive ("Syväsukellus"):
Elmin `Time` moduuli perustuu JavaScriptin `Date`-objektiin, mutta tarjoaa turvallisemman ja funktionaalisemman käyttöliittymän. Historiallisesti päivämäärät ovat olleet monelle ohjelmointikielle haasteellisia muun muassa aikavyöhykkeiden ja karkaussekuntien vuoksi.

Unix Epoch, eli ajanlaskun nollakohta, on valittu sen yksinkertaisuuden ja laajalti tuetun standardin takia. Elm käyttää Posix-aikaa, joka on millisekuntien muodossa oleva kokonaisluku.

Vaikka Elm suoraviivaistaa päivämäärähallintaa, ole tietoinen, että serveri- ja asiakaskoneiden välinen ajanhallinta voi aiheuttaa ongelmia. Aikavyöhykkeet ja kesäaika voivat sekoittaa pakkaa, joten niitä käsitteleviä kirjastoja (kuten `elm/time`) saattaa tarvita kompleksisemmissa sovelluksissa.

## See Also ("Katso myös"):
- Elm Time dokumentaatio: https://package.elm-lang.org/packages/elm/time/latest/
- Elm Browser paketti: https://package.elm-lang.org/packages/elm/browser/latest
- Elm in Action kirja: https://www.manning.com/books/elm-in-action
