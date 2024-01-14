---
title:                "Gleam: Att få den aktuella datumet."
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta den aktuella datumen är en viktig funktion i många program och webbapplikationer. Det gör det möjligt för användare att se när information senast uppdaterades eller för att automatiskt hantera schemalagda uppgifter.

## Så här gör du

För att få den aktuella datumen i Gleam kan du använda "Date" modulen. Först måste du importera modulen och sedan använda funktionen "now" för att hämta den aktuella datumen. Sedan kan du använda andra funktioner, såsom "get_day", "get_month" och "get_year", för att extra information från datumen.

```Gleam
import Date

let current_date = Date.now()

let day = Date.get_day(current_date)
let month = Date.get_month(current_date)
let year = Date.get_year(current_date)

// Output: "Idag är det den 28:e mars 2021."
io.print("Idag är det den " ++ day ++ "e " ++ month ++ " " ++ year ++ ".")
```

## Djupdykning

För de som är intresserade av att lära sig mer om hantering av datum i Gleam, finns det flera andra användbara funktioner i "Date" modulen. Till exempel så kan du använda "format", "parse" och "diff" för att formatera datum, konvertera mellan olika datumformat och beräkna tidsdifferenser.

## Se även

- [Gleam Date Dokumentation](https://gleam.run/lib/date.html)
- [Gleam Standardbibliotek](https://gleam.run/lib/)
- [Gleam Officiell hemsida](https://gleam.run/)