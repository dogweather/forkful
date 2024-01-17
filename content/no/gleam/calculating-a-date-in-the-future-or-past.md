---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Gleam: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Å beregne en dato i framtiden eller fortiden er en viktig funksjon for mange programmerere. Den lar deg forutsi når en hendelse vil skje eller finne ut når en hendelse har funnet sted. Dette er nyttig for å lage tidssensitive funksjoner eller for å samle og analysere historiske data.

# Hvordan:

For å beregne en dato i fremtiden eller fortiden i Gleam, kan du bruke Date-Package biblioteket. Du kan bruke funksjonen `add_days()` for å legge til et bestemt antall dager til en dato, eller `subtract_days()` for å trekke fra dager. Se eksemplene under for å se hvordan dette fungerer.

```Gleam
import Date

let date = Date.from_utc(2021, 10, 10)
let future_date = Date.add_days(date, 30)
let past_date = Date.subtract_days(date, 10)

IO.print("Fremtidig dato:", Date.to_string(future_date))
IO.print("Fortidig dato:", Date.to_string(past_date))

```

Output:

```
Fremtidig dato: 2021-11-09T23:00:00.000-01:00
Fortidig dato: 2021-09-30T23:00:00.000-01:00

```

# Dykk dypere:

Funksjonen for å beregne datoer i framtiden og fortiden er en vanlig funksjon i mange programmeringsspråk og biblioteker. Det har blitt utviklet over tid for å gjøre det enklere for programmere å håndtere og manipulere datoer. Noen andre populære alternativer for å beregne datoer er Chrono og Time bibliotekene.

I Gleam, brukes UTC-tidssone som standard. Dette sikrer at datoene man får fra funksjonene alltid er i riktig tidszone og kan sammenlignes med andre datoer korrekt.

# Se også:

- [Date-Package dokumentasjon](https://gleam.run/packages/wolfie/gleam-date/latest/)
- [Alternativer for å beregne datoer i Rust](https://github.com/chronotope/chrono/issues/207)
- [Innføring i datoer og tidsstyring i programmering](https://www.theserverside.com/feature/How-to-manage-dates-and-times-in-Java-and-while-programming)