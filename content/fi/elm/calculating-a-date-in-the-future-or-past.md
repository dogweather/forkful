---
title:                "Tulevan tai menneen päivämäärän laskeminen"
date:                  2024-01-20T17:31:03.533254-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Mitä ja Miksi?

Laskevaisuuteen tai menneisyyteen sijoittuvien päivämäärien laskeminen mahdollistaa ohjelmiston ajankäytön hallinnan ja suunnittelun. Ohjelmoijat tarvitsevat tätä esimerkiksi aikataulutuksen, muistutusten tai ajanjaksojen laskennan toteuttamiseksi.

# Kuinka:

Elm:ssä päivämäärien käsittelyyn käytetään usein `elm/time` kirjastoa. Alla on esimerkki päivän lisäämisestä nykyiseen aikaan:

```Elm
import Time
import Task
import Browser

getDateFuture : Int -> Task.Task Time.Error Time.Posix
getDateFuture daysToAdd =
    Time.now
        |> Task.andThen (\now -> Task.succeed (Time.add (Time.millisToPosix (daysToAdd * 86400000)) now))

-- Käyttöesimerkki:
main =
    Task.attempt (resultToHtml) (getDateFuture 10)
        
resultToHtml : Result Time.Error Time.Posix -> Browser.Document msg
resultToHtml result =
    case result of
        Ok posix ->
            -- Muunna Posix-aika tarvittaessa
            Browser.sandbox { init = (), update = \_ _ -> (), view = \_ -> Html.text (String.fromInt (Time.posixToMillis posix)) }

        Err _ ->
            Browser.sandbox { init = (), update = \_ _ -> (), view = \_ -> Html.text "An error occurred" }

```

Huomaa, että käyttäessäsi `Time.now`, saat nykyhetken UTC-aikana.

# Syväsukellus:

Elmin päivämääräkäsittely perustuu JavaScriptin Date-objektista saatavaan POSIX-aikaan, joka esittää ajan millisekunteina vuoden 1970 alusta. `elm/time` kirjasto tarjoaa kehyksen ajan hallintaan Elm-koodissa. Vaihtoehtoisia kirjastoja, kuten `justinmimbs/date`, voi käyttää monipuolisempaan päivämääräkäsittelyyn. Näiden kirjastojen implementaatio yksinkertaistaa monimutkaisia tehtäviä, kuten karkausvuosien hallintaa, aikavyöhykelaskentaa ja muotoilua.

# Katso Myös:

- Elm Time kirjaston dokumentaatio: [package.elm-lang.org/packages/elm/time/latest](https://package.elm-lang.org/packages/elm/time/latest)
- Justin Mimbsin Date kirjasto: [package.elm-lang.org/packages/justinmimbs/date/latest](https://package.elm-lang.org/packages/justinmimbs/date/latest)
- Elm-langin opas ajan käsittelystä: [guide.elm-lang.org/effects/time.html](https://guide.elm-lang.org/effects/time.html)