---
title:                "Elm: Omvandling av ett datum till en sträng"
simple_title:         "Omvandling av ett datum till en sträng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng kan vara ett viktigt steg i utvecklingen av Elm-program. Det kan hjälpa till att presentera datum på ett läsbar sätt för användare och underlätta bearbetning av datum i koden.

## Så här gör du

Det finns flera olika metoder för att konvertera ett datum till en sträng i Elm. En enkel metod är att använda funktionen `toString` från modulen `Date`. Här är ett exempel på hur man kan konvertera dagens datum till en sträng:

```
import Date exposing (..)

today : Date
today = Date.fromTime  (Time.millisToPosix 1521552961881)

dateString : String
dateString = toString today

```

Output från funktionen `toString` skulle vara `"2019-03-20"` i detta fall. Man kan också specificera ett format för strängen genom att ange ett andra argument till funktionen `toString` som en `Locale` typ.

```
import Date exposing (..)
import Locale

today : Date
today = Date.fromTime  (Time.millisToPosix 1521552961881)

dateString : String
dateString = toString Locale.svSE "EEEE, d MMMM yyyy" today
```

Detta skulle producera en sträng som säger "tisdag, 20 mars 2019". Mer information om formatering av datumsträngar på ett specifikt språk finns på Elm sidan (https://package.elm-lang.org/packages/elm-lang/core/latest/Date#relying-on-locale)

## Djupdykning

Det är också möjligt att hantera konvertering av datum till strängar på ett mer detaljerat sätt genom att använda funktioner från modulen `Time`. Med funktionen `format` kan man ange ett anpassat format för datumsträngen.

```
import Time exposing (..)

today : Time
today = Time.millisToPosix 1521552961881

dateString : String
dateString = format "YYYY-MM-DD" today
```

Detta skulle producera samma resultat som användningen av `toString` från modulen `Date`.

## Se även

- Dokumentation för modulen `Date`: https://package.elm-lang.org/packages/elm-lang/core/latest/Date 
- Dokumentation för modulen `Time`: https://package.elm-lang.org/packages/elm-lang/core/latest/Time