---
title:                "Gleam: Omvandling av datum till sträng"
simple_title:         "Omvandling av datum till sträng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng är en vanlig uppgift i många programmeringsprojekt. Ofta behöver man visa datum på ett visst format för att till exempel skicka utskick eller spara i en databas. Med Gleam kan du enkelt göra detta utan krångel.

## Hur man gör det

För att konvertera ett datum till en sträng i Gleam använder man funktionen `Date.to_string`. Här är ett exempel på kod som visar hur man kan använda denna funktion:

```Gleam
import Date

let today = Date.from_parts(2021, 9, 1)

let date_string = Date.to_string(today, "%d/%m/%Y")

print(date_string) // 01/09/2021
```

I detta exempel skapar vi ett datum för den första september 2021 och använder sedan `Date.to_string` för att konvertera det till en sträng med formatet "dd/mm/yyyy". Det finns många olika format man kan använda, men vanligtvis består de av en kombination av bokstäver och specialtecken som representerar olika delar av datumet.

## Djupdykning

Det finns en hel del saker man kan göra och tänka på när man arbetar med datum och strängar i Gleam. Här är några tips och trix som kan vara värda att känna till:

- Om du behöver konvertera ett datum till en annan tidszon kan du använda funktionen `Date.shift_timezone` för att justera datumet.
- Om du vill göra flera konverteringar mellan datum och strängar är det en bra idé att skapa en funktion som kan hantera olika format och återanvända den istället för att skriva samma kod flera gånger.
- Tänk på att olika länder och språk har olika format för datum. Om du arbetar med flerspråkiga applikationer kan det vara bra att använda funktionen `Date.format` för att skapa datumsträngar som är anpassade efter användarens inställningar.

## Se även

- [Gleam Language](https://gleam.run)
- [Gleam Date Module](https://gleam.run/modules/date.html)