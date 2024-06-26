---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:05.008585-07:00
description: "Hoe: Elm draait in de browser, dus het heeft geen directe toegang tot\
  \ opdrachtregelargumenten zoals een traditionele server-side of desktop taal dat\
  \ wel\u2026"
lastmod: '2024-03-13T22:44:50.740734-06:00'
model: gpt-4-0125-preview
summary: Elm draait in de browser, dus het heeft geen directe toegang tot opdrachtregelargumenten
  zoals een traditionele server-side of desktop taal dat wel heeft.
title: Commandoregelargumenten lezen
weight: 23
---

## Hoe:
Elm draait in de browser, dus het heeft geen directe toegang tot opdrachtregelargumenten zoals een traditionele server-side of desktop taal dat wel heeft. Echter, ter illustratie, laten we aannemen dat je Elm gebruikt met een server-side framework zoals Node.js via `elm server` of een vergelijkbare opstelling die het doorgeven van argumenten mogelijk maakt. Je code zal de argumenten niet direct afhandelen, maar we zullen het patroon nabootsen:

```Elm
-- Veronderstel binnenkomende argumenten van server-side
type alias Flags = 
    { arg1 : String
    , arg2 : Int
    }

-- Voorbeeld Elm `init` functie gebruikmakend van Flags
init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { defaultModel | passedArg1 = flags.arg1, passedArg2 = flags.arg2 }
    , Cmd.none
    )
```

Voorbeelduitvoer (gestructureerd alsof doorgegeven door de server):

```JSON
{ "arg1": "Hallo", "arg2": 42 }
```

## Diepere Duik
Aangezien Elm een frontend taal is, gaat het traditioneel niet om met opdrachtregelargumenten. Elm werkt in de gecontroleerde omgeving van de browser. De opdrachtregel is een overblijfsel uit de vroege dagen van de informatica, dienend als een venster naar het systeem.

In Node.js of vergelijkbare omgevingen, zou je typisch `process.argv` gebruiken om argumenten te krijgen. Met Elm kom je het dichtst in de buurt met vlaggen wanneer je je Elm-app initialiseert vanuit JavaScript, wat het injecteren van externe data mogelijk maakt. Je accepteert opdrachtregelargumenten indirect via de server-side taal, en vervolgens geef je ze door aan Elm als vlaggen.

Voor diepe integratie, worden Elm-apps gebundeld met server-side code, wat een naadloze ervaring aan gebruikers biedt. Dit patroon van het starten van een Elm programma met specifieke vlaggen is krachtig; het staat toe voor flexibele, dynamische initialisatie die zich aanpast aan verschillende omgevingen en gebruiksscenario's.

## Zie Ook
- Elm's officiële gids over vlaggen: https://guide.elm-lang.org/interop/flags.html
- Node.js documentatie over opdrachtregelargumenten: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- Een voorbeeld van Elm met Node.js: https://medium.com/@_rchaves_/using-elm-with-node-elm-server-side-rendering-via-http-nodejs-and-elm-0-19-6c97f062f7eb
