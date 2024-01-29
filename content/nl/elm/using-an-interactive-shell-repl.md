---
title:                "Het gebruik van een interactieve shell (REPL)"
date:                  2024-01-28T22:08:51.853224-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het gebruik van een interactieve shell (REPL)"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
De Read-Eval-Print Loop (REPL) is een eenvoudige, interactieve programmeeromgeving die individuele gebruikersinvoer neemt, deze evalueert en het resultaat aan de gebruiker teruggeeft. Elm-programmeurs gebruiken REPL voor snelle experimenten, debugging of het leren van de taal.

## Hoe te:
Elm wordt niet geleverd met een geïntegreerde REPL. Echter, je kunt `elm repl` vanaf je opdrachtregel gebruiken om een Elm-sessie te starten nadat je Elm hebt geïnstalleerd.

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

In deze sessie, na het importeren van List functies, verdubbelden we de getallen in een lijst en kregen we direct het resultaat.

## Diepere Duik
Elm's REPL kan beperkt lijken vergeleken met die van sommige andere talen zoals Python of JavaScript, aangezien Elm een gecompileerde taal is die gericht is op het produceren van webapps. Historisch gezien heeft Elm zich gericht op volledige applicaties in plaats van scripting of shell-interacties.

Alternatieven voor Elm's REPL zijn `elm-live` en online editors zoals Ellie waar je wijzigingen in code real-time in een browser kunt zien.

Wat implementatie betreft, compileert de Elm REPL snippets van Elm-code naar JavaScript op de achtergrond, waardoor je Elm interactief kunt uitvoeren. Dit verschilt van REPLs van geïnterpreteerde talen, die deze compilatiestap niet nodig hebben. Elm REPL is ook gestript om de kern van de taal lichtgewicht en gefocust te houden.

## Zie Ook
- Elm's officiële gids over interactiviteit: https://guide.elm-lang.org/interop/
- Ellie, een online Elm-speelplaats: https://ellie-app.com/new
- `elm-live`, een flexibele ontwikkelserver voor Elm: https://www.elm-live.com/
