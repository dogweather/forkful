---
title:                "Elm: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Elm on monipuolinen ja intuitiivinen ohjelmointikieli, joka sopii erityisen hyvin verkkosivujen kehittämiseen. Lataamalla verkkosivun Elm-koodilla, saat mahdollisuuden tuottaa dynaamista ja interaktiivista sisältöä käyttäjille.

## Miten

Käyttäen Elm-koodia voit ladata verkkosivun sisällön helposti ja nopeasti. Käytämme tähän tarkoitukseen `Http` -moduulia. Alla on esimerkki koodista, joka lataa ja tulostaa verkkosivun sisällön konsoliin.

```Elm
module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode
import String

type Msg 
    = PageLoaded (Result Http.Error String)

init : () -> (Model, Cmd Msg)
init _ = ((), Http.getString "https://www.example.com")

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        PageLoaded result ->
            case result of 
                Ok content -> 
                    ((), Cmd.none) 
                    -- Tulostaa verkkosivun sisällön konsoliin
                    (Debug.log "Verkkosivun sisältö:" content)
                Err error -> 
                    ((), Cmd.none)
                        -- Tulostaa virheilmoituksen konsoliin
                        (Debug.log "Virhe:" (Debug.toString error))

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

view : Model -> Html Msg
view _ = 
    p [] [ text "Loading page..."]

main = Browser.sandbox { init = init, update = update, view = view, subscriptions = subscriptions }
``` 

Kun suoritat tämän koodin, näet tulos-konsolissa verkkosivun sisällön. 

```
https://www.example.com
```

## Syvemmälle

Koodiesimerkissä käytetty `Http.getString` -funktio ottaa yksinkertaisesti verkkosivun URL-osoitteen ja palauttaa `Result` -tuloksen, joka voi olla joko `Ok` tai `Err` riippuen siitä, onko sivun lataaminen onnistunut vai ei. Elm tarjoaa myös muita `Http` -moduulin funktioita, kuten `Http.post`, jotka antavat sinulle vielä enemmän hallintaa lataamisprosessissa.

## Katso myös

- [Elm viralliset dokumentit](https://guide.elm-lang.org)
- [Elm Suomen käyttäjäyhteisö](https://elm-suomi.fi/)
- [Elm Suomen Slack-kanava](https://elmsuomi.slack.com)