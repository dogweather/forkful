---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:08.461977-07:00
description: "Debuggen in Elm betreft het identificeren en verwijderen van fouten\
  \ uit je code. Programmeurs doen dit om te zorgen dat hun applicaties correct werken\
  \ en\u2026"
lastmod: 2024-02-19 22:05:09.783144
model: gpt-4-0125-preview
summary: "Debuggen in Elm betreft het identificeren en verwijderen van fouten uit\
  \ je code. Programmeurs doen dit om te zorgen dat hun applicaties correct werken\
  \ en\u2026"
title: Een debugger gebruiken
---

{{< edit_this_page >}}

## Wat & Waarom?
Debuggen in Elm betreft het identificeren en verwijderen van fouten uit je code. Programmeurs doen dit om te zorgen dat hun applicaties correct werken en om de codekwaliteit te verbeteren. Elm's sterke typesysteem vangt veel problemen op tijdens het compileren, maar tools voor runtime debugging zijn essentieel voor het gladstrijken van logische fouten en onverwachte gedragingen.

## Hoe te:
Elm heeft niet een ingebouwde debugger in de traditionele zin zoals JavaScript dat bijvoorbeeld heeft met browserontwikkelaarstools. Echter, de Elm-gemeenschap heeft tools ontwikkeld om dit gat op te vullen. Hier is hoe je `elm-debug-transformer` kunt gebruiken om je Elm-app te debuggen:

```Elm
-- Installeer elm-debug-transformer (Node-pakket)

1. npm install -g elm-debug-transformer

-- Gebruik elm-debug-transformer om je app te starten

2. elm-debug-transformer --port=8000 jouwHoofdElmBestand.elm 
```

Eenmaal draaiende, maakt `elm-debug-transformer` een WebSocket-verbinding aan voor het loggen. Je zult debuginformatie zien in de console van je browser waar je de datastructuren van je programma kunt inspecteren op gegeven punten in je applicatie.

In Elm 0.19 en later kunnen de functies van het `Debug`-module, zoals `Debug.log` en `Debug.todo`, je helpen met het traceren van waarden en het bewust markeren van onafgemaakte delen van je code. Hier is hoe je Debug.log gebruikt:

```Elm
import Debug

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( Debug.log "Verhogend" { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( Debug.log "Verlagend" { model | count = model.count - 1 }, Cmd.none )
```

Je zult "Verhogend" of "Verlagend" berichten zien in de console van je browser samen met de nieuwe staat van het `model`.

## Diepere Duik
Evan Czaplicki, de maker van Elm, wilde een taal maken waarin gangbare bugs onmogelijk of makkelijk te vangen zouden zijn. Deze filosofie is waarom Elm's kern geen traditionele debugfuncties bevat. Elm's statische analyse en type-inferentie dragen enorm bij aan het verminderen van runtime fouten, wat de behoefte aan geavanceerde runtime debugging vermindert. Historische alternatieven omvatten het gebruik van het nu verouderde `elm-reactor`, dat time-travel debugging bood – een manier om acties in je app terug te spoelen en opnieuw af te spelen.

Vandaag de dag helpen tools zoals `elm-debug-transformer` en het gebruik van Elm's `Debug`-module de kloof te overbruggen. Terwijl het `Debug`-module bedoeld is voor gebruik tijdens ontwikkeling alleen en moet worden verwijderd voor productiebuilds, is het een onschatbare tool voor het precies lokaliseren en loggen van staatveranderingen.

Houdt in gedachten dat traditionele JavaScript debugging-technieken, zoals breakpoints of stap-voor-stap uitvoering, niet direct toepasbaar zijn in Elm vanwege zijn architectuur en de Elm-runtime die statusupdates afhandelt. Elm moedigt je aan je programma te structureren zodat de datastroom duidelijk is en volgt strikte types en onveranderlijkheidsgaranties, wat het aantal gevallen waarin debugging nodig is minimaliseert.

## Zie Ook
- Elm's officiële gids over het afhandelen van runtime-uitzonderingen: https://guide.elm-lang.org/error_handling/
- `elm-debug-transformer` GitHub-repository: https://github.com/kraklin/elm-debug-transformer
- Elm-discussiedraad die debugstrategieën bespreekt: https://discourse.elm-lang.org/c/show-and-tell/debugging
- Elm's `Debug`-module documentatie: https://package.elm-lang.org/packages/elm/core/latest/Debug
