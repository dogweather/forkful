---
title:                "Gleam: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Det er mange grunner til å ville beregne en dato i fremtiden eller fortiden. Kanskje du ønsker å lage en applikasjon som planlegger hendelser, eller kanskje du vil vite nøyaktig hvor mange dager det har vært siden en spesiell begivenhet. Uansett hva årsaken er, så kan Gleam hjelpe deg med å enkelt og nøyaktig beregne datoer.

## Slik gjør du det
Det første du må gjøre er å importere `Calendar` modulen fra Gleam-standard biblioteket. Deretter kan du bruke funksjonene `add/2` og `subtract/2` for å legge til eller trekke fra et gitt antall dager, uker, måneder eller år fra en gitt dato.

```Gleam
import Calendar

let today = Calendar.Date.create(2021, 9, 15)

let threeDaysFromToday = Calendar.add(today, Calendar.Duration.days(3))

let tenYearsAgo = Calendar.subtract(today, Calendar.Duration.years(10))

io.print("Tre dager fra i dag er \(Calendar.Date.to_string(threeDaysFromToday))")
// Utoutput: Tre dager fra i dag er 18. september 2021

io.print("Ti år tilbake i tid er \(Calendar.Date.to_string(tenYearsAgo))")
// Utoutput: Ti år tilbake i tid er 15. september 2011
```

Du kan også bruke funksjonene `add_duration/2` og `subtract_duration/2` for å legge til eller trekke fra et bestemt antall enheter til en gitt dato.

```Gleam
import Calendar

let today = Calendar.Date.create(2021, 9, 15)
let duration = Calendar.Duration{ days: 6, months: 2, years: 1 }

let futureDate = Calendar.add_duration(today, duration)

let pastDate = Calendar.subtract_duration(today, duration)

io.print("Datoen seks dager, to måneder og ett år fra nå er \(Calendar.Date.to_string(futureDate))")
// Utoutput: Datoen seks dager, to måneder og ett år fra nå er 27. november 2022

io.print("Datoen seks dager, to måneder og ett år tilbake i tid er \(Calendar.Date.to_string(pastDate))")
// Utoutput: Datoen seks dager, to måneder og ett år tilbake i tid er 9. juli 2020
```

## Dypdykk
Gleam's `Calendar` modul bruker Gregorian-kalenderen og håndterer også skuddår og tidssoner. Hvis du trenger å arbeide med andre typer kalendere, kan du undersøke biblioteket [Calen](https://github.com/oestrich/calen) som gir støtte for flere kalendere i Gleam.

## Se også
- [Gleam-standard bibliotekets `Calendar` modul](https://hexdocs.pm/gleam_stdlib/Calendar.html)
- [Calen biblioteket](https://github.com/oestrich/calen)
- [Offisiell Gleam dokumentasjon](https://gleam.run/documentation/)