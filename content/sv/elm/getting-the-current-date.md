---
title:                "Hämta aktuellt datum"
html_title:           "Elm: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att få den aktuella datumen är en viktig del av programmering eftersom det gör det möjligt att hålla reda på tiden och datumet på ett exakt och tillförlitligt sätt. Detta kan vara användbart för att skapa tidstämplar, planera uppgiftsutföranden eller för att visa användaren det aktuella datumet i en applikation.

## Hur man gör:
För att få den aktuella datumen i Elm används funktionen `Time.now`. Det returnerar ett `Task` som utförs asyncronously för att få nuvarande datum. Nedan finns ett exempel på hur man använder denna funktion och hur utdata kan se ut:
 
```Elm
import Time

-- Funktionen som använder Time.now
getCurrentDate : Task x Time.Posix
getCurrentDate =
    Time.now


-- Anrop till funktionen och utskrift av resultatet
Time.perform
    (\_ -> Debug.log "Aktuellt datum är" (Time.format "%Y-%m-%d" (Time.now |> Time.toTime)))
    (\_ -> getCurrentDate)
```

Utskriften från koden ovan skulle se ut som följande:

`Aktuellt datum är "2021-10-20"` 

## Djupdykning:
Att få det aktuella datumet har varit en viktig funktion i programmering sedan dess början. Tidigare var det mycket svårare att få tag på detta eftersom det krävde manuell hantering och beräkningar. Idag finns det dock många olika möjligheter för att enkelt få den aktuella datumen, både i Elm och andra programmeringsspråk.

En alternativ metod för att få det aktuella datumet i Elm är att använda paketet `chronology` som erbjuder flera funktioner för att arbeta med datum och tider. Det är värt att undersöka olika alternativ för att hitta det som passar bäst för ditt specifika projekt och användningsområde.

När det kommer till implementationen av `Time.now` så använder sig Elm av Javascripts `Date` objekt under huven för att få tag på det aktuella datumet. Det är därför viktigt att notera att tiden som returneras är baserad på användarens aktuella tidszon och inställningar. Detta kan vara bra att tänka på vid utveckling av internationella applikationer eller när exakt tidszon är viktigt.

## Se även:
För mer information om `Time` biblioteket i Elm och hur man kan använda det mer avancerade `TimeZone` paketet, se dokumentationen på Elm hemsida [Elm Time Library](https://package.elm-lang.org/packages/elm/time/latest/).

För att lära dig mer om `Date` objektet i Javascript finns det många resurser tillgängliga online, bland annat på [Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date).