---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Lataaminen web-sivulta on prosessi, jossa noudetaan tietoa, kuten HTML-sisältöä, verkkosivulta ohjelmaan. Ohjelmoijat tekevät näin analysoimaan ja hyödyntämään sisältöä ohjelmissaan.

## Miten Toimia:

Aloitamme HTTP-pyynnön tekemiselle Elm-ohjelmassa. Laitetaan tämä tapahtumaan kun koko ohjelma käynnistyy.

```Elm
import Browser
import Html exposing (Html)
import Http

main =
  Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { url: String
  , response: String
  }

init : Model
init =
  { url = "http://example.com"
  , response = ""
  }

type Msg
  = GotResponse (Result Http.Error String)

update : Msg -> Model -> Model
update msg model =
  case msg of
    GotResponse result ->
      case result of
        Ok body ->
          { model | response = body }

        Err _ ->
          model

view : Model -> Html Msg
view model =
  Html.text model.response

subscriptions : Model -> Sub Msg
subscriptions _ =
  Http.getString "http://example.com"
    |> Http.send GotResponse
```

Tulosteessa näet ladatun www-sivun sisällön.

## Sukellus Syvemmälle:

Ohjelmistokehittäjät ovat lataamassa sivuja verkkosivuilta jo Internetin alkuaikoina. Elm tarjoaa sisäänrakennetun Http-paketin tätä tarkoitusta varten, joka on suoraviivaista käyttää.

Alternatiivisia tapoja on, mukaan lukien kolmannen osapuolen kirjastot kuten `elm-http-extra`. Se on hyvä, jos tarvitset monimutkaisempaa logiikkaa tai konfiguraatiota pyynnöissäsi.

Elmin implementaatio noudattaa tiukasti funktionaalista ohjelmointiparadigmaa. Http-pyyntöjen tulokset käsitellään viesteinä, mikä sopii hyvin Elmin arkkitehtuuriin.

## Katso Myös:

- [Elmin virallinen Http-paketti dokumentaatio](https://package.elm-lang.org/packages/elm/http/latest/)
- [Elm-http-extra kirjasto](https://package.elm-lang.org/packages/lukewestby/elm-http-extra/latest/)
- [Elmin virallinen opas](https://guide.elm-lang.org/)
- [Elm Discourse](https://discourse.elm-lang.org/) - Are we missing something? You can find more discussion about Elm here.