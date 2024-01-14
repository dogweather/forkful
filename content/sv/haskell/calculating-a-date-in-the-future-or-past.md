---
title:                "Haskell: Beräkna ett datum i framtiden eller i det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller i det förflutna"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Att kunna beräkna ett datum i framtiden eller i det förflutna kan vara användbart i många olika programmeringsprojekt. Det kan hjälpa till att automatisera uppgifter eller visa information på ett mer lättförståeligt sätt för användare.

## Hur man gör det

Det finns flera bibliotek tillgängliga på Haskell som kan hjälpa till att beräkna datum. Ett populärt val är "time" biblioteket, som ger många funktioner för att hantera datum och tid.

Först måste vi importera biblioteket och sedan definiera ett datum som vi vill beräkna från. I detta exempel kommer vi att använda det aktuella datumet.

```Haskell
import Data.Time
idag <- getCurrentTime
```

För att beräkna ett datum i framtiden eller förflutna måste vi använda oss av funktionen "addDays" som tar ett heltal, antalet dagar som vi vill lägga till eller dra bort, och returerar ett nytt datum.

För att beräkna ett datum tio dagar framåt från dagens datum skulle koden se ut som följande:

```Haskell
framtidigtDatum <- addDays 10 idag
```

Om vi vill beräkna ett datum ett år bakåt från dagens datum skulle det se ut så här:

```Haskell
forflutetDatum <- addDays (-365) idag
```

När vi har vårt nya datum kan vi sedan sedan använda det i vårt program. Här är ett exempel där vi skriver ut det beräknade datumet på skärmen:

```Haskell
putStrLn $ "Framtida datum: " ++ show framtidigtDatum
```

Resultatet av detta program skulle bli:

```
Framtida datum: 2021-03-28 12:00:00 UTC
```

## Djupdykning

Att kunna beräkna datum är nyckeln till att skapa mer dynamiska och anpassningsbara program. Men det finns också flera andra funktioner i "time" biblioteket som kan vara användbara när det gäller att hantera datum och tid i ditt program.

En sådan funktion är "diffDays" som tar två datum som argument och beräknar antalet dagar mellan dem. Detta kan vara användbart för att jämföra datum eller för att beräkna tidsperioder mellan olika händelser.

```Haskell
diffDays forflutetDatum framtidigtDatum
```

Djupdykningen innehåller också information om andra bibliotek för datumhantering i Haskell, till exempel "datetime" och "chronos".

## Se även

- [Time bibliotekets dokumentation](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Ett exempel på att hantera datum med datetime biblioteket](https://github.com/jonathanknowles/date-haskell/blob/master/src/MiniTime.hs)
- [Haskell Chronos bibliotekets GitHub-sida](https://github.com/snoyberg/chronos)