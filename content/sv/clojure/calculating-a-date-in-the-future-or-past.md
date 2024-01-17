---
title:                "Beräkning av ett datum i framtiden eller det förflutna"
html_title:           "Clojure: Beräkning av ett datum i framtiden eller det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att beräkna ett datum i framtiden eller förfluten tid handlar om att få en datobaserad värde för ett visst antal dagar, månader eller år framåt eller bakåt från ett givet datum. Programmerare använder detta ofta för att hantera tidsberoende data eller skapa tidslinjer för projekt.

## Hur man gör:
```Clojure
; För att beräkna ett datum i framtiden: 
(java.time.LocalDate/plus (java.time.LocalDate/now) (java.time.Period/ofDays 30))
; Output: #object[java.time.LocalDate 0x6d500aa1 "2020-07-11"]

; För att beräkna ett datum i förfluten tid: 
(java.time.LocalDate/minus (java.time.LocalDate/now) (java.time.Period/ofMonths 6))
; Output: #object[java.time.LocalDate 0x70435479 "2019-12-12"]
```

## Djupdykning:
Historiskt sett har människor behövt kunna beräkna datum i förfluten tid för att hålla reda på tiden och säsonger. Alternativ till att använda Clojure för att beräkna datum är att använda inbyggda funktioner i olika programmeringsspråk eller att använda speciella bibliotek som Joda-Time. I Clojure sker beräkningar av datum genom att använda klassen LocalDate och metoder som plus och minus.

## Se även:
- https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- https://github.com/nablex/thorina.jodatime