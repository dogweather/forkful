---
title:                "Att påbörja ett nytt projekt"
html_title:           "Elm: Att påbörja ett nytt projekt"
simple_title:         "Att påbörja ett nytt projekt"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

### Vad & Varför?
Att starta ett nytt projekt innebär att du skapar en ny Elm-applikation från grunden. Detta kan innebära att du utvecklar en helt ny applikation eller att du bygger vidare på en befintlig applikation. Programerare börjar ofta nya projekt för att utforska nya idéer, lösa problem eller förbättra befintliga applikationer.

### Så här:
```Elm
module Main exposing (..)

import Html exposing (text)

main =
    text "Hello World!"
```

Kör detta exempel i Elm REPL eller kompilera det till JavaScript och öppna det i en webbläsare. Du bör se texten "Hello World!" visas på skärmen. Detta är det grundläggande kodmönstret för en Elm-applikation.

### Djupdykning:
Elm är ett funktionellt programmeringsspråk som utvecklades av Evan Czaplicki år 2012. Det är baserat på Haskell och utformats för att skapa användargränssnitt för webbläsaren. Alternativ till Elm kan vara JavaScript, PureScript eller ReasonML, men Elm erbjuder en renare och säkrare kodstruktur. För att starta ett Elm-projekt behöver du installera Elm-plattformen på din dator och börja med att skriva kod i en Elm-fil (.elm). För att se mer avancerade exempel och funktioner, kan du utforska Elm's officiella dokumentation.

### Se också:
[Elm Documentaion](https://elm-lang.org/docs)