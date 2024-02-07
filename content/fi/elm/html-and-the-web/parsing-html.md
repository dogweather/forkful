---
title:                "HTML:n jäsennys"
date:                  2024-02-03T19:12:08.672307-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML:n jäsennys"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä ja miksi?
HTML:n jäsentäminen Elm-kielessä tarkoittaa tietojen poimimista HTML-dokumenteista. Ohjelmoijat tekevät näin, jotta he voivat käyttää web-sisältöä tai käyttöliittymiä, jotka palauttavat HTML-muotoista tietoa. Tämä mahdollistaa interaktiivisempien ja dynaamisempien web-sovellusten luomisen.

## Kuinka tehdään:
Elmissä ei ole suoraan HTML:n jäsentämiseen sisäänrakennettua kirjastoa samalla tavalla kuin JavaScriptissä tai Pythonissa, johtuen sen korostuksesta tyypiturvallisuudesta ja ajonaikaisten virheiden välttämisestä. Voit kuitenkin käyttää `Http`-pyyntöjä sisällön noutamiseen ja sitten käyttää säännöllisiä lausekkeita tai palvelinpuolen käsittelyä tarvittavien tietojen poimimiseen. Monimutkaisempaan HTML:n jäsentämiseen yleinen lähestymistapa on käyttää omistettua taustapalvelua HTML:n jäsentämiseen ja datan palauttamiseen muodossa, jonka Elm voi suoraan käsitellä, kuten JSON.

Tässä on esimerkki HTML-sisällön noutamisesta (olettaen, että palvelimen vastaus on selkeässä muodossa tai erityisen tagin sisältö):

```elm
import Browser
import Html exposing (Html, text)
import Http

type alias Model =
    { content : String }

initialModel : Model
initialModel =
    { content = "" }

type Msg
    = Fetch
    | ReceiveContent String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Fetch ->
            ( model
            , Http.get
                { url = "https://example.com"
                , expect = Http.expectString ReceiveContent
                }
            )

        ReceiveContent content ->
            ( { model | content = content }
            , Cmd.none
            )

view : Model -> Html Msg
view model =
    text model.content

-- Oletetaan, että pääfunktio ja tilausten määrittelyt noudattavat Elmin standardin sovellusrakennetta.
```

Vastauksen käsittelyyn todellisen elementtien tai tietojen jäsentämiseksi saatat harkita HTML-sisällön lähettämistä hallinnoimallesi palvelinpäätepisteelle, jossa voit käyttää kielissä kuten JavaScript (Cheerio, Jsdom) tai Python (BeautifulSoup, lxml) saatavilla olevia kirjastoja jäsentämiseen, ja sitten palauttaa rakenteellista dataa (kuten JSON) takaisin Elm-sovellukseesi.

Muista, että HTML:n suora jäsentäminen client-side Elm-koodissa ei ole tyypillistä johtuen kielirajoituksista ja filosofiasta, joka kannustaa selkeään eroon sisällön noudon ja sisällön käsittelyn välillä. Elmin arkkitehtuuri suosii datan käsittelyä turvallisemmassa, ennustettavammassa muodossa, kuten JSON.
