---
title:                "Elm: Beräkning av ett datum i framtiden eller förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller förflutna"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Ibland behöver vi kunna räkna ut ett datum i framtiden eller i det förflutna inom Elm programmering. Det kan vara för att planera händelser eller skapa dynamiska tidslinjer. I detta blogginlägg kommer vi att utforska hur man gör detta.

## Så här gör du
För att räkna ut ett datum i Elm använder vi modulen `Time`. Vi kan behöva importera den med kommandot `import Time` i början av vår kod. Sedan kan vi använda funktionen `add` för att lägga till ett visst antal enheter (minuter, timmar, dagar, etc.) till ett befintligt datum. Här är ett exempel på hur man beräknar ett datum 3 dagar framåt:

```Elm
add Time.days 3 (Time.now)
```

Detta skulle returnera ett datum som är 3 dagar efter detta aktuella datum. Vi kan även använda negativa värden för att beräkna ett datum i det förflutna. Här är ett exempel:

```Elm
add Time.months -2 (Time.now)
```

Detta skulle ge oss ett datum som är 2 månader före det aktuella datumet. Det finns också andra funktioner som `sub` och `addHours` som kan användas för mer specifika beräkningar.

## Djupdykning
När man beräknar datum i Elm är det viktigt att veta hur datatypen `Time` fungerar. Den representerar tiden som antalet millisekunder sedan 1 januari 1970. Detta är även känt som Unix-epoken. Genom att lägga till eller subtrahera ett visst antal enheter från detta nummer kan vi beräkna ett nytt datum.

Det är också värt att notera att tiden som representeras av datatypen `Time` är oberoende av användarens tidszon. Detta är viktigt att tänka på när man arbetar med internationella applikationer.

## Se även
- [Elm Time dokumentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Time Demo](https://elm-lang.org/examples/time)
- [Beräkna datum i Elm för olika tidszoner](https://discourse.elm-lang.org/t/how-to-calculate-date-for-time-zone/1880)