---
title:                "Att starta ett nytt projekt"
html_title:           "Arduino: Att starta ett nytt projekt"
simple_title:         "Att starta ett nytt projekt"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att starta ett nytt projekt är processen att initialisera ett programmeringsarbete från grunden. Programmerare gör detta för att bygga unika lösningar, skapa nya produkter eller lära sig nya programmeringskoncept.

## Så här gör du:
Här är hur du kan starta ett nytt Elm-projekt. Först, installera Elm och skapa en ny katalog för ditt projekt.

```Elm
$ npm install -g elm
$ mkdir my-elm-project && cd my-elm-project
```

Ange därefter följande kod för att skapa din första Elm-fil.

```Elm
$ echo "module Main exposing (..)\n\nimport Html exposing (text)\n\nmain =\n    text \"Hej, Elm!\"" >> Main.elm
```

Om du kör Elm-filen kommer du att se "Hej, Elm!" på skärmen.

```Elm
$ elm reactor
```

Navigera till `http://localhost:8000` för att se projektet.

## Djupdykning:

(1) Historisk kontext: Elm är ett funktionellt programmeringsspråk som fokuserar på användargränssnitt. Lanserad 2012, det var det första att förespråka arkitektonisk modell känd som "The Elm Architecture".

(2) Alternativ: Förutom Elm, finns det andra verktyg för att bygga webbapplikationer som React, Vue och Angular. Men, Elm skilldrar sig genom sina robusta säkerhetsfunktioner och enklare koncept för tillståndshantering.

(3) Implementeringsdetaljer: Ett Elm-program består av moduler, vilket bidrar till att främja koden återanvändning och underhåll. Dessutom använder Elm ett strikt statiskt typsystem, vilket betyder att många potentiella körningsfel fångas vid kompileringstid. 

## Se Även:
Du kan lära dig mer om Elm-programmering genom att besöka följande länkar:

- Elm's officiella dokumentation: https://elm-lang.org/docs
- Elm Architecture: https://guide.elm-lang.org/architecture/
- Elm tutorial på Elm's officiella Youtube-kanal: https://www.youtube.com/watch?v=tdYVjgVqgi4