---
title:                "Omvandla en sträng till gemener"
html_title:           "Arduino: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Omvandla en Sträng till Gemener med Elm

## Vad och Varför?
At omvandla en värdet i sträng till gemener innebär att förändra alla tecken till deras gemena motsvarigheter. Detta tillåter en programmerare att jämföra strängar utan att oroa sig för om de ursprungligen skrevs med gemener eller versaler.

## Hur man Gör:
Elm erbjuder en mycket enkel funktion för att göra detta, nämligen `String.toLower`. Titta på följande exempel
```Elm
module Main exposing (..)

import Html exposing (text)

main = text (String.toLower "Hej VÄRLDEN!")
```
Om du kör detta skript, kommer utdata att var "hej världen!".

## Fördjupning
`String.toLower` funktion är en del av den standard Elm kärnbiblioteket, som har varit där i många år. Den är baserad på samma koncept som finns i alla stora högnivå programmeringsspråk. Men, precis som med allt i Elm, fungerar det lite annorlunda. Elm är ett funktionellt programmeringsspråk, så du behöver inte oroa dig för några oväntade sidoeffekter när du använder `String.toLower`.

Det finns andra sätt att omvandla en sträng till gemener, till exempel genom att använda `List.map` tillsammans med `Char.toLower` på en lista av tecken (det vill säga, en sträng). Men `String.toLower` är det mest lämpliga sättet att göra detta i Elm.

## Se Även
* Elm's String API dokumentation: https://package.elm-lang.org/packages/elm/core/latest/String.
* Utforska mer om Elm's funktionell programmeringsparadigm: https://guide.elm-lang.org/core_language.html. 

Använda Elm för att göra strängen operationer är ganska enkelt och intuitivt. Nu ska du vara redo att omvandla alla tecken i en sträng till gemener i Elm!