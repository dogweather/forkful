---
date: 2024-01-26 03:49:17.623503-07:00
description: "Elm ei sis\xE4ll\xE4 k\xE4\xE4nt\xE4j\xE4\xE4n sis\xE4\xE4n rakennettua\
  \ vianj\xE4ljitint\xE4 perinteisess\xE4 mieless\xE4, kuten esimerkiksi JavaScript\
  \ tarjoaa selaimen kehitt\xE4j\xE4ty\xF6kaluissa.\u2026"
lastmod: '2024-03-13T22:44:56.493276-06:00'
model: gpt-4-0125-preview
summary: "Elm ei sis\xE4ll\xE4 k\xE4\xE4nt\xE4j\xE4\xE4n sis\xE4\xE4n rakennettua\
  \ vianj\xE4ljitint\xE4 perinteisess\xE4 mieless\xE4, kuten esimerkiksi JavaScript\
  \ tarjoaa selaimen kehitt\xE4j\xE4ty\xF6kaluissa.\u2026"
title: "Debuggerin k\xE4ytt\xF6"
weight: 35
---

## Kuinka:
Elm ei sisällä kääntäjään sisään rakennettua vianjäljitintä perinteisessä mielessä, kuten esimerkiksi JavaScript tarjoaa selaimen kehittäjätyökaluissa. Kuitenkin Elm-yhteisö on rakentanut työkaluja tämän aukon täyttämiseksi. Näin voit käyttää `elm-debug-transformer` -työkalua Elm-sovelluksesi vianjäljitykseen:

```Elm
-- Asenna elm-debug-transformer (Node-paketti)

1. npm install -g elm-debug-transformer

-- Käytä elm-debug-transformer -työkalua sovelluksesi käynnistämiseen

2. elm-debug-transformer --port=8000 yourMainElmFile.elm 
```

Kun `elm-debug-transformer` on käynnissä, se luo WebSocket-yhteyden lokiin kirjaamista varten. Näet selaimen konsolissa vianjäljitystietoja, joista voit tarkastella ohjelmasi tietorakenteita sovelluksesi eri vaiheissa.

Elmissä 0.19 ja uudemmissa, `Debug`-moduulin funktiot, kuten `Debug.log` ja `Debug.todo`, voivat auttaa arvojen jäljityksessä ja koodin tarkoituksella keskeneräisiksi merkittyjen osien ilmoittamisessa. Näin käytät Debug.log:

```Elm
import Debug

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( Debug.log "Lisäämässä" { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( Debug.log "Vähentämässä" { model | count = model.count - 1 }, Cmd.none )
```

Näet selaimen konsolissa viestit "Lisäämässä" tai "Vähentämässä" yhdessä `model`in uuden tilan kanssa.

## Syväsukellus
Elmin luojan, Evan Czaplickin, tavoitteena oli luoda kieli, jossa yleiset virheet olisivat mahdottomia tai helppoja havaita. Tämä filosofia on syy siihen, miksi Elmin ydin ei sisällä perinteisiä vianjäljitystoimintoja. Elmin statiikan analyysi ja tyyppipäätteleminen vähentävät valtavasti ajonaikaisia virheitä, mikä vähentää kehittyneiden ajonaikaisten vianjäljitystyökalujen tarvetta. Aikaisempi vaihtoehto oli nyt vanhentunut `elm-reactor`, joka tarjosi aikamatkavianjäljitystä – tapa kelata ja toistaa toimintoja sovelluksessasi.

Nykyään työkalut kuten `elm-debug-transformer` ja Elmin `Debug`-moduulin käyttö auttavat paikkaamaan tätä aukkoa. Vaikka `Debug`-moduuli on tarkoitettu vain kehitysvaiheen käyttöön ja se tulisi poistaa ennen tuotantoversioiden rakentamista, se on korvaamaton väline tilan muutosten tarkkaan määrittämiseen ja kirjaamiseen.

Pidä mielessä, että perinteiset JavaScript-vianjäljitystekniikat, kuten katkaisupisteet tai askel askeleelta suorittaminen, eivät ole suoraan sovellettavissa Elmissä johtuen sen arkkitehtuurista ja Elm-ajonaikaisen tilan päivitysten käsittelystä. Elm rohkaisee sinua rakentamaan ohjelmasi niin, että datan kulku on selkeää ja seuraa tiukkoja tyyppi- ja muuttumattomuustakuita, minimoimalla tilanteet, joissa vianjäljitystä tarvitaan.

## Katso myös
- Elmin virallinen opas ajonaikaisten poikkeusten käsittelyyn: https://guide.elm-lang.org/error_handling/
- `elm-debug-transformer` GitHub-repositorio: https://github.com/kraklin/elm-debug-transformer
- Elm-keskusteluketju, joka käsittelee vianjäljitysstrategioita: https://discourse.elm-lang.org/c/show-and-tell/debugging
- Elmin `Debug`-moduulin dokumentaatio: https://package.elm-lang.org/packages/elm/core/latest/Debug
