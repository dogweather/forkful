---
title:    "Elm: Jämföring av två datum"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är ett vanligt problem inom programmering när man arbetar med tidsbaserade applikationer. Det kan vara användbart för att sortera eller filtrera data, beräkna åldrar eller för att visa olika meddelanden baserat på datum.

## Hur man gör det

För att jämföra två datum i Elm använder man funktionen `compare`, som tar två datum som argument och returnerar en `Order` typ. Den kan sedan användas för att avgöra om det första datumet är före, efter eller lika med det andra.

```
Elm  2011-05-14 |>  compare  (2011-05-12) == GT
Elm  2011-05-12 |>  compare  (2011-05-12) == EQ
Elm  2011-05-10 |>  compare  (2011-05-12) == LT
```

Man kan också använda funktionen `max` och `min` för att välja det senaste respektive tidigaste datumet från en lista av datum.

```
Elm  max [2011-05-11, 2011-05-12, 2011-05-13] == 2011-05-13
Elm  max [] ==  Nothing
```

## Djupdykning

När man jämför datum är det viktigt att vara medveten om att det inte bara handlar om att jämföra det faktiska datumet, utan också tiden och tidszonen. Om man till exempel jämför en tidzonavhängig datumtyp (som `Date`) med en tidszonberoende typ (som `Posix`), kan man få oönskade resultat. Det är därför viktigt att i sådana fall konvertera de två datumtyperna till samma format innan man jämför dem.

Det finns också flera användbara funktioner för att göra mer komplexa jämförelser av datum, som `isSameDay`, `isBefore`, `isAfter` och `withinRange`. Dessa funktioner kan användas för att avgöra om två datum är samma dag, om ett datum är före eller efter ett annat eller om ett datum ligger inom en viss tidsperiod.

## Se även

- [Elm dokumentation om Date](https://package.elm-lang.org/packages/elm/time/latest/Time#Date)
- [Elm paket för att arbeta med datum](https://package.elm-lang.org/packages/elm-community/date-extra/latest/)
- [Blogginlägg om att arbeta med datum i Elm](https://terezka.cz/elm/date-utilities/)