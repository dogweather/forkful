---
title:                "Schrijven naar standaardfout"
aliases:
- nl/elm/writing-to-standard-error.md
date:                  2024-01-28T22:13:42.698844-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schrijven naar standaardfout"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Schrijven naar standaardfout (stderr) is het uitvoeren van foutmeldingen en diagnostiek gescheiden van reguliere output. Programmeurs doen dit om applicaties te debuggen en te monitoren zonder foutmeldingen te vermengen met de standaarduitvoer (stdout).

## Hoe:
Elm draait op het web, en browsers maken geen onderscheid tussen stdout en stderr zoals command-line interfaces dat doen. Je kunt echter stderr simuleren met behulp van JavaScript-interoperabiliteit via poorten. Zo stel je het in:

```Elm
port module Main exposing (..)

import Html

-- Definieer een poort om foutmeldingen naar JavaScript te sturen
port stderr : String -> Cmd msg

-- Functie om het schrijven naar stderr te simuleren
writeToStdErr : String -> Cmd msg
writeToStdErr message =
    stderr message

main =
    writeToStdErr "Fout: Er is iets misgegaan"
    |> Html.programWithFlags { init = \_ -> ((), Cmd.none), update = \_ _ -> ((), Cmd.none), view = \_ -> Html.text "", subscriptions = \_ -> Sub.none }
```

En de bijbehorende JavaScript:

```JavaScript
var app = Elm.Main.init();

// Luister naar fouten op de 'stderr'-poort en log ze als fouten in de console
app.ports.stderr.subscribe(function(message) {
    console.error(message);
});
```

Voorbeelduitvoer in de browserconsole:

```
Fout: Er is iets misgegaan
```

## Uitdieping
Historisch gezien is stderr een Unix-concept waarbij uitvoerstromen worden gecategoriseerd voor betere procescontrole en automatisering. Elm, voornamelijk een frontend-taal, heeft geen ingebouwde ondersteuning voor dit concept aangezien webapplicaties typisch fouten afhandelen binnen de UI of via netwerkoperaties, niet via een terminal. Alternatieven voor debuggen in Elm omvatten het gebruik van de Elm Debugger, die visueel de staat van je applicatie presenteert. Achter de poorten construeert Elm's JavaScript-interoperabiliteit berichten waarop JavaScript abonneert, waardoor in wezen de kloof tussen Elm en traditionele stderr wordt overbrugd.

## Zie Ook
- Elm's officiële gids over poorten: https://guide.elm-lang.org/interop/ports.html
- Elm Debugger: https://guide.elm-lang.org/effects/debugging.html
- Schrijven van cross-platform stdout en stderr in Node.js: https://nodejs.org/api/console.html
