---
title:    "Elm: Att hämta aktuellt datum"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Varför

När vi utvecklar webbapplikationer, är det ofta nödvändigt att inkludera datum och tid i vår kod. Att få den aktuella datumen eller tiden kan vara användbart för att visa live-uppdaterade information, skapa tidslinjer eller kalendrar, eller för att hålla reda på hur länge sedan ett visst evenemang inträffade. I denna bloggpost ska vi prata om hur man kan få den aktuella datumen i Elm.

## Hur man gör det

För att få den aktuella datumen i Elm, behöver vi använda Elm's `Time` modul. Först måste vi importera modulen i vår kod. Sedan kan vi använda funktionen `Time.now` för att få den aktuella datumen som en `Posix` tid. Sedan kan vi använda `Time.toDate` för att konvertera det till ett läsbart datum-objekt. Här är ett exempel:

```Elm
import Time exposing (..)

currentDate : Date
currentDate =
    Time.now
        |> Time.toDate
```

`Date` är ett typ som tillhandahålls av `Time` modulen och innehåller attribut för år, månad och dag. Om vi ​​vill ha både datum och tid, kan vi använda `Time.toMillis` för att få en tidsstämpel och sedan konvertera den till ett datum-objekt med `Time.millisToDate`.

## Djupdykning

Om du är intresserad av att fördjupa dig i hur Elm hanterar datum och tid, kan det vara värt att titta närmare på `Time` modulen. Det finns många användbara funktioner som t.ex. `Time.inDays` eller `Time.inSeconds` som kan hjälpa dig att manipulera och konvertera olika tidsenheter. Dessutom erbjuder Elm inga inbyggda sätt att manipulera tidszoner, men det finns bibliotek som kan hjälpa till med detta.

## Se även

- [Elm Time-modulen](https://package.elm-lang.org/packages/elm/time/latest)
- [Elm's offentliga datum-typ](https://package.elm-lang.org/packages/elm/time/latest/Time#Date)
- [Elm tidsstämpel-typ](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix)
- [Elm's offentliga tidsstämpel-typ](https://package.elm-lang.org/packages/elm/time/latest/Time#Millis)