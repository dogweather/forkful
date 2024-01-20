---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att extrahera delsträngar handlar om att ta ut en specifik del av en sträng baserat på index. Detta är ett användbart verktyg när programmerare behöver bearbeta eller analysera datan i en viss sträng.

## Så här gör du:
Elm erbjuder `String.slice` funktionen för att extrahera substrängar. Låt oss se på ett exempel:

```Elm
import Html exposing (text)

main =
    "Hej Världen!" 
        |> String.slice 0 3
        |> text
```

I detta exempel kommer outputen bli "Hej". `String.slice` tar två index, början och slutet, och returnerar delen av strängen mellan dessa index.

## Djupdykning
Elm är relativt nytt och bygger på influenser från många andra programmeringsspråk. Metoden för att extrahera substrängar är dock ganska standardiserad över de flesta språk. Alternativ till `String.slice` kan vara `String.left` eller `String.right` för att få en substräng från början eller slutet av strängen. 

Elm implementerar dessa funktioner genom att först konvertera strängen till en lista med tecken, utför operationer på listan, och sedan konvertera tillbaka till en sträng. Detta är effektivt för programmeringsspråk men kan bli tidskrävande för stora mängder data.

## Se även
För mer information, kolla in dessa dokument:
- [Elm String API](https://package.elm-lang.org/packages/elm/core/latest/String) för en heltäckande visning av alla strängfunktioner i Elm.
- [Elm Guide](https://guide.elm-lang.org/) för en övergripande introduktion till Elm, inklusive en sektion om strängar.