---
title:                "Att extrahera substrängar"
html_title:           "Elm: Att extrahera substrängar"
simple_title:         "Att extrahera substrängar"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera substrängar är när man tar ut en del av en sträng och använder den som en egen sträng. Detta är en vanlig teknik som används av programmerare för att manipulera och hantera data på ett effektivt sätt.

## Så här gör du:
```Elm
-- Extracting substring from start
String.left 3 "Hej hej hej" == "Hej"

-- Extracting substring from end
String.right 3 "Hej hej hej" == "hej"

-- Extracting substring from specific index
String.slice 4 7 "Hej hopp" == "hopp"
```

## Djupdykning:
Att extrahera substrängar är en teknik som har funnits länge inom programmering och används fortfarande flitigt idag. Innan dess var det vanligt att manuellt loopa igenom en sträng för att hitta och extrahera den önskade delen. Alternativ till att extrahera substrängar inkluderar att använda regex eller andra metoder för strängmanipulering. I Elm sker detta genom att använda funktioner som `String.left`, `String.right` och `String.slice`.

## Se även:
- [Elm string documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [W3Schools substring tutorial](https://www.w3schools.com/jsref/jsref_substr.asp)
- [MDN substring reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)