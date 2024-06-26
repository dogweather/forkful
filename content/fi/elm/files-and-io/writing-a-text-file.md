---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:10.712298-07:00
description: "Kuinka: Koska Elm toimii selaimessa ja on suunniteltu olemaan puhdas\
  \ ohjelmointikieli ilman sivuvaikutuksia, sill\xE4 ei ole suoraa p\xE4\xE4sy\xE4\
  \u2026"
lastmod: '2024-03-13T22:44:56.507121-06:00'
model: gpt-4-0125-preview
summary: "Koska Elm toimii selaimessa ja on suunniteltu olemaan puhdas ohjelmointikieli\
  \ ilman sivuvaikutuksia, sill\xE4 ei ole suoraa p\xE4\xE4sy\xE4 tiedostoj\xE4rjestelm\xE4\
  \xE4n."
title: Tekstitiedoston kirjoittaminen
weight: 24
---

## Kuinka:
Koska Elm toimii selaimessa ja on suunniteltu olemaan puhdas ohjelmointikieli ilman sivuvaikutuksia, sillä ei ole suoraa pääsyä tiedostojärjestelmään. Näin ollen tiedostoon kirjoittaminen tyypillisesti sisältää datan lähettämisen ulos JavaScriptiin porttien kautta. Tässä on ohjeet, miten voit sen tehdä:

1. **Määrittele porttimoduuli tekstin lähettämiseksi JavaScriptiin:**

```elm
port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- Määritä portti lähettämään tekstidataa JavaScriptiin
port saveText : String -> Cmd msg

-- Pää näkymä
view : Html msg
view =
    div []
        [ button [ onClick (saveText "Hei, Elm kirjoittaa tiedostoon!") ] [ text "Tallenna tiedostoon" ]
        ]

-- Tilauksen asetus (ei käytetä tässä esimerkissä, mutta vaaditaan porttimodulissa)
subscriptions : model -> Sub msg
subscriptions _ =
    Sub.none

-- Sovelluksen asetus
main : Program () model msg
main =
    Browser.element
        { init = \_ -> ((), Cmd.none)
        , view = \_ -> view
        , update = \_ _ -> ((), Cmd.none)
        , subscriptions = subscriptions
        }
```

2. **Toteuta vastaava JavaScript-koodi:**

HTML-tiedostossasi tai JavaScript-moduulissasi, käsittele Elm-sovelluksen portti tekstin tallentamiseksi. Voisit käyttää `FileSaver.js` kirjastoa tiedoston tallentamiseen asiakkaan puolella tai lähettää datan palvelimelle käsiteltäväksi.

```javascript
// Olettaen, että Elm.Main.init() on jo kutsuttu ja sovellus on käynnissä
app.ports.saveText.subscribe(function(text) {
    // Käyttäen FileSaver.js tallentamaan tiedostoja asiakkaan puolella
    var blob = new Blob([text], {type: "text/plain;charset=utf-8"});
    saveAs(blob, "esimerkki.txt");
});
```

Esimerkkituloste ei ole suoraan sovellettavissa, koska tulos on tiedoston luominen, mutta napin painamisen jälkeen Elm-sovelluksessasi, tietokoneellesi pitäisi latautua tiedosto nimeltä "esimerkki.txt", joka sisältää merkkijonon "Hei, Elm kirjoittaa tiedostoon!".

Tässä lähestymistavassa Elm:n ja JavaScriptin välinen kommunikaatio on elintärkeää. Vaikka Elm pyrkii sisältämään mahdollisimman paljon sovelluksesi logiikasta, yhteistyö JavaScriptin kanssa porttien kautta mahdollistaa tehtävien, kuten tiedostoon kirjoittamisen, suorittamisen, jota Elm ei suoraan tue. Muista, Elm:n puhtaus ja turvallisuus vahvistuvat tällä mallilla, varmistaen, että Elm-sovelluksesi pysyvät helposti ylläpidettävinä ja niistä on helppo saada selvää, jopa kun ne vuorovaikuttavat monimutkaisen ulkomaailman kanssa.
