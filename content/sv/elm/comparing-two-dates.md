---
title:                "Jämföring av två datum"
html_title:           "Elm: Jämföring av två datum"
simple_title:         "Jämföring av två datum"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum kan vara användbart när man vill få information om hur länge något har pågått, till exempel hur länge ett projekt har varit igång eller hur lång tid det har tagit att slutföra en uppgift. Det kan också vara användbart när man vill sortera eller filtrera data baserat på datum.

## Hur man gör det

Först och främst behöver vi importera Elm-paketet "Time" för att kunna arbeta med datum. Sedan kan vi använda funktionen "compare" för att jämföra två datum, vilket ger oss en "Order" som kan vara "LT" (mindre än), "EQ" (lika med) eller "GT" (större än).

```elm
import Time exposing (..)

date1 = fromCalendarDate 2020 7 21
date2 = fromCalendarDate 2019 3 10

order = compare date1 date2
-- Returnerar "GT"
```

Vi kan också använda funktionerna "isBefore" och "isAfter" för att enkelt se om ett datum är före eller efter ett annat.

```elm
isBefore date1 date2
-- Returnerar "False"

isAfter date1 date2
-- Returnerar "True"
```

## Djupdykning

När vi jämför två datum, tar Elm hänsyn till både datum och tid. Det betyder att om vi till exempel har datumet 2020-07-21 klockan 15:00 och jämför med datumet 2020-07-21 klockan 12:00, så kommer det att anses som "GT" då det är ett senare klockslag.

Det är också viktigt att notera att funktionerna "compare", "isBefore" och "isAfter" är baserade på ISO8601-standard, vilket innebär att de fungerar bäst med fullständiga datum och tider. Om vi bara använder funktionen "fromCalendarDate" som i exempel 1, kommer tiden att sättas till midnatt, vilket kan påverka jämförelserna.

## Se även

* [Elm Time-paketet](https://package.elm-lang.org/packages/elm/time/latest/)
* [ISO8601-standard](https://en.wikipedia.org/wiki/ISO_8601)