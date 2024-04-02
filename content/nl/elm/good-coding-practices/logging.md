---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:43.229816-07:00
description: "Loggen is in wezen het proces van het vastleggen van gebeurtenissen\
  \ en data-uitvoer van een softwareprogramma tijdens het draaien, denk erover als\
  \ het\u2026"
lastmod: '2024-03-13T22:44:50.731574-06:00'
model: gpt-4-0125-preview
summary: "Loggen is in wezen het proces van het vastleggen van gebeurtenissen en data-uitvoer\
  \ van een softwareprogramma tijdens het draaien, denk erover als het\u2026"
title: Logboekregistratie
weight: 17
---

## Wat & Waarom?
Loggen is in wezen het proces van het vastleggen van gebeurtenissen en data-uitvoer van een softwareprogramma tijdens het draaien, denk erover als het dagboek van de software. Programmeurs gebruiken loggen om bij te houden wat er onder de motorkap gebeurt - het is van onschatbare waarde voor het debuggen van problemen, het in realtime monitoren van het systeemgedrag en het analyseren van eerdere activiteiten voor prestatieoptimalisaties of audits.

## Hoe te:
De architectuur van Elm ondersteunt geen neveneffecten zoals loggen uit de doos - je handelt ze af via commando's, die een onderdeel zijn van de architectuur van je applicatie. Voor educatieve doeleinden, laten we bekijken hoe je loggen kunt simuleren door berichten naar JavaScript te sturen via poorten.

Eerst definieer je een poortmodule:

```Elm
port module Logger exposing (..)

-- Definieer een poort om logs naar JavaScript te sturen
port log : String -> Cmd msg
```

In je `Main.elm` gebruik je de `log` poort om een logbericht uit te zenden:

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    geval msg van
        AnEvent ->
            -- hier enkele updates aan je model
            ( updatedModel, log "AnEvent heeft plaatsgevonden." )

        AnotherEvent ->
            -- hier andere updates aan je model
            ( anotherUpdatedModel, log "AnotherEvent heeft plaatsgevonden." )
```

Aan de JavaScript-kant zou je je abonneren op de `log` poort om de binnenkomende logberichten af te handelen:

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

Voorbeelduitvoer in de JavaScript-console zou dan zijn:

```
AnEvent heeft plaatsgevonden.
AnotherEvent heeft plaatsgevonden.
```

## Diepgaand
Traditioneel, in talen zoals Python of Java, wordt het loggen gedaan door een logbibliotheek te gebruiken, die een eenvoudige API biedt om berichten te loggen op verschillende niveaus zoals debug, info, waarschuwing, fout en kritiek.

Elm, met zijn focus op zuiverheid en onveranderlijkheid, biedt dit soort direct loggen niet, omdat elk type IO of neveneffect duidelijk wordt beheerd door de Elm-architectuur.

Wanneer je volwaardig loggen in Elm nodig hebt, vertrouw je doorgaans op externe JavaScript-tools. Poorten, zoals hierboven getoond, zijn de brug naar deze tools. Het Debug-moduul is een andere optie, maar dat is alleen bedoeld voor ontwikkelingsgebruik en niet voor productielogging.

Naast poorten maken programmeurs vaak gebruik van Elm's compilerberichten en runtime debugfaciliteiten, zoals `Debug.log`, die je in je code kunt invoegen om waarden te traceren. Het omhult een uitdrukking en logt de uitvoer ervan naar de console zoals:

```Elm
view model =
    Debug.log "Model Debug" model
    -- hier je viewcode
```

Dit is echter ook niet bedoeld voor productie. Tools zoals elm-logger bieden enkele abstracties over poorten voor loggen, hoewel deze ook meer bedoeld zijn voor ontwikkeling dan voor productie.

## Zie Ook
- Elm poorten: https://guide.elm-lang.org/interop/ports.html
- Elm `Debug`: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Elm-discussie over loggen: https://discourse.elm-lang.org/t/elm-and-logging/546
- JavaScript Console API: https://developer.mozilla.org/nl/docs/Web/API/Console
- elm-logger pakket: https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
