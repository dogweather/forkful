---
title:    "Gleam: Att få den aktuella datumen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att få den nuvarande datumen är en av de mest grundläggande funktionerna i programmering. Det är en viktig del av att skapa dynamiska applikationer och hålla reda på tidsstämplar i olika processer.

## Hur man gör

För att få den nuvarande datumen i Gleam, behöver vi använda funktionen `Date.now()` som returnerar ett värdet motsvarande aktuell tid i millisekunder. För att omvandla detta till ett mer läsbart format, kan vi använda funktionen `Date.toString()` som returnerar ett strängvärde av datumet enligt ett specifikt format.

```Gleam
let current_date = Date.now()
let readable_date = Date.toString(current_date, "%Y-%m-%d") 
```

Detta kommer att tilldela det nuvarande datumen i formatet "år-månad-dag" till variabeln `readable_date`.
Vi kan också specificera ett mer detaljerat format genom att använda placeholders som `%H` för timmar, `%M` för minuter och `%S` för sekunder.

## Djupdykning

Det finns många fler funktioner som kan användas för att manipulera eller omvandla datum i Gleam. Till exempel `Date.add()` för att lägga till ett visst antal millisekunder till ett datum, `Date.difference()` för att räkna antalet millisekunder mellan två datum och `Date.from_string()` för att konvertera ett datum från en sträng till ett gleam Date-värde.

Det är också möjligt att använda bibliotek som tillhandahåller mer avancerade funktioner för datummanipulering, som till exempel [gleam-time](https://github.com/gleam-lang/gleam-time).

## Se också

- [Dokumentation för Gleams Date-modul](https://gleam.run/modules/date.html)
- [Gleam-timelib-Dokumentation](https://github.com/gleam-lang/gleam-time)
- [Gleam-discussion about date handling](https://github.com/gleam-lang/gleam/discussions/2223)