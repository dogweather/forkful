---
date: 2024-01-26 03:49:06.311494-07:00
description: "Feils\xF8king i Elm involverer \xE5 identifisere og fjerne feil fra\
  \ koden din. Programmerere gj\xF8r det for \xE5 sikre at applikasjonene deres fungerer\
  \ korrekt og\u2026"
lastmod: '2024-03-13T22:44:40.713620-06:00'
model: gpt-4-0125-preview
summary: "Feils\xF8king i Elm involverer \xE5 identifisere og fjerne feil fra koden\
  \ din. Programmerere gj\xF8r det for \xE5 sikre at applikasjonene deres fungerer\
  \ korrekt og\u2026"
title: "\xC5 bruke en feils\xF8ker"
---

## Hvordan:
Elm har ikke en innebygd feilsøker i tradisjonell forstand som, si, JavaScript gjør med nettleserutviklingsverktøy. Imidlertid har Elm-samfunnet utviklet verktøy for å fylle dette gapet. Slik kan du bruke `elm-debug-transformer` for å feilsøke Elm-appen din:

```Elm
-- Installer elm-debug-transformer (Node-pakke)

1. npm install -g elm-debug-transformer

-- Bruk elm-debug-transformer for å starte appen din

2. elm-debug-transformer --port=8000 dittHovedElmFiler.elm 
```

Når `elm-debug-transformer` kjører, oppretter den en WebSocket-tilkobling for logging. Du vil se feilsøkingsinformasjon i nettleserens konsoll der du kan inspisere programmets datastrukturer på gitte punkter i applikasjonen din.

I Elm 0.19 og senere kan funksjoner fra `Debug`-modulen som `Debug.log` og `Debug.todo` hjelpe deg med å spore verdier og bevisst merke uferdige deler av koden din. Slik bruker du Debug.log:

```Elm
import Debug

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( Debug.log "Øker" { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( Debug.log "Minker" { model | count = model.count - 1 }, Cmd.none )
```

Du vil se "Øker" eller "Minker" meldinger i nettleserens konsoll sammen med den nye tilstanden til `model`.

## Dypdykk
Elms skaper, Evan Czaplicki, hadde som mål å lage et språk der vanlige feil ville være umulige eller enkle å fange opp. Denne filosofien er grunnen til at Elms kjerne ikke inkluderer tradisjonelle feilsøkingsfunksjoner. Elms statiske analyse og typeinferens bidrar massivt til å redusere kjøretidsfeil, noe som reduserer behovet for avansert feilsøking under kjøretid. Historiske alternativer inkluderte bruk av den nå avviklede `elm-reactor` som tilbød feilsøking med tidsreise - en måte å spole tilbake og gjenspille handlinger i appen din.

I dag hjelper verktøy som `elm-debug-transformer` og bruken av Elms `Debug`-modul til å bro over gapet. Selv om `Debug`-modulen er ment for bruk under utvikling kun og bør fjernes før produksjonsbygg, er den et uvurderlig verktøy for å identifisere og logge tilstandsforandringer.

Husk at tradisjonelle JavaScript-feilsøkingsteknikker, som punktstopp eller trinn-for-trinn-utførelse, ikke er direkte anvendelige i Elm på grunn av dens arkitektur og Elm runtime-håndtering av tilstandsoppdateringer. Elm oppmuntrer deg til å strukturere programmet ditt slik at dataflyten er klar og følger strenge typer og immutabilitetsgarantier, noe som minimerer tilfellene der feilsøking er nødvendig.

## Se Også
- Elms offisielle guide om håndtering av kjøretidsunntak: https://guide.elm-lang.org/error_handling/
- `elm-debug-transformer` GitHub-repositorium: https://github.com/kraklin/elm-debug-transformer
- Elm-diskusjonstråd som diskuterer feilsøkingsstrategier: https://discourse.elm-lang.org/c/show-and-tell/debugging
- Elms `Debug`-moduldokumentasjon: https://package.elm-lang.org/packages/elm/core/latest/Debug
