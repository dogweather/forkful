---
date: 2024-01-20 15:14:14.752219-07:00
description: "Saada nykyinen p\xE4iv\xE4m\xE4\xE4r\xE4 tarkoittaa kellonaikaan sidotun\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4tiedon hy\xF6dynt\xE4mist\xE4. K\xE4yt\xE4mme t\xE4t\xE4\
  \ toiminnallisuutta esimerkiksi lokeissa,\u2026"
lastmod: '2024-03-09T21:11:38.723927-07:00'
model: unknown
summary: "Saada nykyinen p\xE4iv\xE4m\xE4\xE4r\xE4 tarkoittaa kellonaikaan sidotun\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4tiedon hy\xF6dynt\xE4mist\xE4. K\xE4yt\xE4mme t\xE4t\xE4\
  \ toiminnallisuutta esimerkiksi lokeissa,\u2026"
title: "Nykyisen p\xE4iv\xE4m\xE4\xE4r\xE4n hankkiminen"
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
