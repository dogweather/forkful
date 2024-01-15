---
title:                "Omvandla ett datum till en sträng"
html_title:           "Gleam: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng kan vara användbart för att visa datum på ett läsbart sätt eller för att utföra beräkningar med datum. Det är också vanligt för datavetare och utvecklare att arbeta med datum i program.

## Så här gör du

För att konvertera ett datum till en sträng i Gleam behöver du först importera Time-paketet. Sedan kan du använda funktionen `format` för att formatera ditt datum enligt önskat format. Här är ett exempel:

```Gleam
import gleam/time

// Skapa ett datum
let date = Time.make(2021, 6, 1)

// Konvertera till en sträng med formatet YYYY-MM-DD
let formatted_date = Time.format(date, "%Y-%m-%d")

// Skriv ut resultatet
gleam/io.print("Datum: " ++ formatted_date)

// Output: Datum: 2021-06-01
```

För att få en fullständig lista över möjliga formatalternativ kan du kolla in dokumentationen för Time-paketet. Notera att det också finns andra funktioner för att konvertera datum som `DateTime.to_string` och `Date.to_string`.

## Djupdykning

För att förstå hur konvertering av datum till strängar fungerar i Gleam kan det vara bra att veta att datum och tidsinformation representeras som tal i form av timestamp, där 1 sekund motsvarar 1000 millisekunder. Detta innebär att det finns en algoritm som omvandlar ett timestamp till en läsbar representation baserat på det önskade formatet.

Du kan också märka att vissa formatalternativ är unika för Gleam, till exempel `%L` som står för millisekunder. Detta kan vara användbart för exakta tidsberäkningar eller för att visa millisekunder i en loggning.

## Se även

- [Gleam Time Package Documentation](https://hexdocs.pm/gleam/gleam.time.html)
- [Gleam Formatting String Reference](https://hexdocs.pm/gleam/gleam.time.html#module-formatting-strings)