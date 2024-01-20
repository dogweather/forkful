---
title:                "Läsa kommandoradsargument"
html_title:           "Bash: Läsa kommandoradsargument"
simple_title:         "Läsa kommandoradsargument"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa kommandoradsargument är metoden att ta emot input från terminalen när du kör ditt program. Det hjälper programmerare att göra sina program flexibla och mer adaptiva till olika användarbehov.

## Så här gör du:
Tyvärr stöder det aktuella Elm (0.19) inte direkt läsning av kommandoradsargument. Elm är en funktionell programmeringsspråk för frontend-webben och hanterar inte samma I/O som andra backend-språk. Men vi kan använda `Node.js` för att direkt ta kommandoradsargument och skicka det till vår Elm applikation.

Först, skapa en fil `index.js` för att köra din Elm-kod:

```Javascript
var Elm = require('./elm/Main');
var app = Elm.Main.init({
    flags: process.argv
});
```

Sedan i din Elm `Main.elm`:

```Elm
module Main exposing (..)

import Platform

type alias Flags = List String 

main =
    Platform.worker
        { init = init
        , subscriptions = always Sub.none
        }

init : Flags -> ((), Cmd Msg)
init flags =
    ( (), Cmd.none )
```

Dit `init` tar argumenten från kommandoraden och Elm tar emot de som flaggor.

## Djupgående Diver
Historiskt sett har att läsa kommandoradsargument i programmering erbjudit flexibilitet och makt för operativsystemanvändare att interagera med program. Alternativ i Elm inkluderar skapande av fler interaktiva användargränssnitt eller sömlös kommunikation med serverskript för att arbeta runt dess I/O-begränsningar. Intressant nog är Elm's unika implementering till stor del baserad på dess mål att förbättra pålitlighet och underhållbarhet i webbapplikationskod.

## Se Även
[Node.js dokumentation om `process.argv`](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
[Elm Guide om Flaggor](https://guide.elm-lang.org/interop/flags.html)
[Community Diskussion om Kommandoradsargument på Elm Discourse](https://discourse.elm-lang.org/t/command-line-arguments-in-elm/1988)