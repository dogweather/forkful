---
title:    "Elm: Beräkna ett datum i framtiden eller förflutna"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Varför

Att kunna beräkna ett datum i framtiden eller förflutna kan vara användbart för att planera eller hålla koll på tidsbaserade händelser i din Elm-applikation. Det kan också vara en användbar funktion för att ge användare feedback baserat på ett specifikt datum.

# Så här gör du

För att kunna räkna ut ett datum i framtiden eller förflutna i din Elm-kod, behöver du först importera "Date" från biblioteket "Time". Sedan kan du använda funktionen "add" för att addera eller subtrahera antal dagar, månader eller år från ett given datum. Till exempel:

```Elm
import Date exposing (Date)
import Time

todaysDate : Date
todaysDate = Time.now

tomorrowsDate : Date
tomorrowsDate = Date.add 1 Time.day todaysDate

yesterdaysDate : Date
yesterdaysDate = Date.sub 1 Time.day todaysDate

```

I detta exempel importeras "Date" och "Time" biblioteket och definieras sedan tre variabler: "todaysDate" som är dagens datum, "tomorrowsDate" som är morgondagens datum och "yesterdaysDate" som är gårdagens datum. Genom att använda funktionen "add" och "sub" kan vi enkelt lägga till eller ta bort en dag från det givna datumet.

# Djupdykning

För att kunna räkna ut ett datum i förflutna eller framtiden på ett mer exakt sätt, kan du använda funktionen "fromCalendarDate". Denna funktion tar in år, månad och dag som argument och returnerar ett datum i form av en "Maybe Date". Detta gör det möjligt att hantera eventuella fel eller ogiltiga datum som kan uppstå. Till exempel:

```Elm
import Date exposing (fromCalendarDate)

myDate : Maybe Date
myDate = fromCalendarDate 2020 8 30

```

Här definieras variabeln "myDate" som representerar 30 augusti 2020. Om detta datum är ogiltigt, till exempel om det inte finns en 30:e dag i augusti, kommer funktionen att returnera "Nothing". Annars kommer den att returnera "Just myDate" där "myDate" är ett giltigt datum i form av "Date".

# Se även

- [Official Elm Time library](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Date documentation](https://package.elm-lang.org/packages/elm/time/latest/Date)