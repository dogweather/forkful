---
date: 2024-01-26 01:03:10.678966-07:00
description: "Lokitus on pohjimmiltaan prosessi, jossa tallennetaan tapahtumia ja\
  \ ohjelmiston tuottamia tietoja sen suorituksen aikana, ajattele sit\xE4 ohjelmiston\u2026"
lastmod: '2024-03-13T22:44:56.495172-06:00'
model: gpt-4-1106-preview
summary: "Lokitus on pohjimmiltaan prosessi, jossa tallennetaan tapahtumia ja ohjelmiston\
  \ tuottamia tietoja sen suorituksen aikana, ajattele sit\xE4 ohjelmiston p\xE4iv\xE4\
  kirjana."
title: Lokitus
weight: 17
---

## Mikä & Miksi?
Lokitus on pohjimmiltaan prosessi, jossa tallennetaan tapahtumia ja ohjelmiston tuottamia tietoja sen suorituksen aikana, ajattele sitä ohjelmiston päiväkirjana. Ohjelmoijat käyttävät lokitusta seuratakseen, mitä konepellin alla tapahtuu - se on korvaamaton apuväline ongelmien selvittämisessä, järjestelmän käyttäytymisen seurannassa reaaliaikaisesti sekä menneen toiminnan analysoinnissa suorituskyvyn optimointia tai tarkastuksia varten.

## Kuinka:
Elmin arkkitehtuuri ei tue sivuvaikutuksia kuten lokitusta valmiina - käsittelet niitä komentojen kautta, jotka ovat osa sovelluksesi arkkitehtuuria. Koulutustarkoituksessa, katsotaan miten voisit simuloida lokitusta lähettämällä viestejä JavaScriptiin porttien kautta.

Aluksi määrittelet porttimoduulin:

```Elm
port module Logger exposing (..)

-- Määrittele portti, joka lähettää lokitietoja JavaScriptiin
port log : String -> Cmd msg
```

`Main.elm`-tiedostossasi käyttäisit `log`-porttia lähettääksesi lokiviestin:

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- joitakin päivityksiä malliisi täällä
            ( updatedModel, log "AnEvent tapahtui." )

        AnotherEvent ->
            -- muita mallipäivityksiä täällä
            ( anotherUpdatedModel, log "AnotherEvent tapahtui." )
```

JavaScript-puolella tilaisit `log`-portin vastaanottamaan saapuvat lokiviestit:

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

Esimerkkilokituloste JavaScript-konsolissa olisi sitten:

```
AnEvent tapahtui.
AnotherEvent tapahtui.
```

## Syväsukellus
Perinteisesti, kielissä kuten Python tai Java, lokitusta tehdään käyttämällä lokitus-kirjastoa, joka tarjoaa suoraviivaisen API:n viestien lokittamiseen eri tasolla kuten debug, info, varoitus, virhe ja kriittinen.

Elm, keskittyessään puhtauteen ja muuttumattomuuteen, ei tarjoa tätäntyyppistä suoraa lokitusta, sillä kaikki IO-toiminnot tai sivuvaikutukset hallitaan erikseen Elmin arkkitehtuurin kautta.

Kun tarvitset täysin varusteltua lokitusta Elm:ssä, luotat tyypillisesti ulkoisiin JavaScript-työkaluihin. Portit, kuten yllä näytettiin, ovat silta näihin työkaluihin. Debug-moduuli on toinen vaihtoehto, mutta se on tarkoitettu vain kehityskäyttöön, ei tuotantolokien keräämiseen.

Porttien lisäksi ohjelmoijat käyttävät usein Elmin kääntäjäviestejä ja ajonaikaisia vianetsintämahdollisuuksia, kuten `Debug.log`, jonka voit lisätä koodiisi arvojen jäljittämiseksi. Se käärii ilmaisun ja lokittaa sen tulosteen konsoliin näin:

```Elm
view model =
    Debug.log "Mallin Debug" model
    -- koodisi näkymälle täällä
```

Tämäkään ei kuitenkaan ole tarkoitettu tuotantokäyttöön. Työkalut, kuten elm-logger, tarjoavat jonkin verran abstraktioita porteille lokittamisen osalta, vaikkakin nämä ovat myös enemmän tarkoitettu kehityskäyttöön kuin tuotantoon.

## Katso Myös
- Elm portit: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Elm keskustelu lokituksesta: https://discourse.elm-lang.org/t/elm-and-logging/546
- JavaScript Console API: https://developer.mozilla.org/en-US/docs/Web/API/Console
- elm-logger paketti: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
