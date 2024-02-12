---
title:                "Extrahera delsträngar"
aliases:
- /sv/elm/extracting-substrings.md
date:                  2024-01-20T17:45:38.760040-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extrahera delsträngar"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Vad & Varför?)
Att extrahera delsträngar innebär att plocka ut specifika delar av en sträng. Programmerare gör det för att manipulera och använda textdata på mer detaljerade sätt.

## How to: (Hur gör man?)
Elm håller det enkelt. Här är ett exempel på hur du extraherar delsträngar med funktionen `String.slice`:

```Elm
import String exposing (slice)

-- Vi ska plocka ut "världen" från strängen "Hej världen!"
main =
  let
    fullText = "Hej världen!"
    partText = slice 4 11 fullText
  in
  -- partText blir "världen"
  text partText
```

Kör koden, och du får en enkel output: "världen".

## Deep Dive (Djupdykning)
I tidiga programmeringsspråk som C måste du jobba direkt med minnet för att hantera strängar, vilket var knepigare och mer felbenäget. Elm, som är modernare, döljer komplexiteten och låter oss göra sådant här lätt och säkert. 

Alternativen finns. Du kan använda `String.left` och `String.right` för att beskära strängar från start eller slut istället för `String.slice`. Dock är `slice` mer flexibel.

Angående implementation, Elm hanterar strängar internt som UTF-16, vilket är viktigt att ha i åtanke om du arbetar med tecken utanför ASCII. Det kan påverka indexeringen eftersom vissa tecken kan ta mer än ett "ord" i minnet.

## See Also (Se även)
- Elm's officiella dokumentation om strängar: https://package.elm-lang.org/packages/elm/core/latest/String
- En genomgång av strängmanipulation i Elm: https://elmprogramming.com/string.html
- UTF-16-teckenuppslag: https://en.wikipedia.org/wiki/UTF-16
