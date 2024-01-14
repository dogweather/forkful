---
title:                "Gleam: Å få dagens dato"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få den nåværende datoen er en vanlig oppgave når du koder. Det kan være nyttig for å vise brukeren når en bestemt hendelse skjedde, eller for å holde styr på tidsbegrensede funksjoner.

## Slik gjør du det

For å få den nåværende datoen i Gleam, kan du bruke funksjonen `Date.now()` som returnerer et `DateTime` objekt. Du kan deretter bruke denne datoen til å hente ut informasjon som dag, måned eller år ved å bruke funksjoner som `DateTime.day()`, `DateTime.month()` eller `DateTime.year()`.

```Gleam
let nåværende_dato = Date.now()

IO.println("Dagens dato er: " ++ nåværende_dato.year() ++ "-" ++ nåværende_dato.month() ++ "-" ++ nåværende_dato.day())
```

Dette vil gi følgende utdata:

```bash
Dagens dato er: 2020-09-25
```

## Dykk dypere

I Gleam, er `DateTime` objektet basert på UTC-tid, som er en standardisert tidssone som brukes for å sammenligne tider på tvers av landegrenser. Dette kan være viktig for å unngå feil i kodingen din, spesielt hvis du jobber med internasjonale team eller prosjekter.

Det er også verdt å merke seg at mens `DateTime` objektet kun representerer dato og tid, kan du kombinere det med Gleam sine innebygde funksjoner for å få en mer detaljert formattert utdata. For eksempel kan du bruke funksjonen `Regex.replace()` for å endre formatet på datoen.

## Se også

- [Offisiell Gleam dokumentasjon for Date-modulen](https://gleam.run/modules/date.html)
- [Offisiell Gleam dokumentasjon for DateTime-modulen](https://gleam.run/modules/date_time.html)
- [Offisiell Gleam kildekode for Date og DateTime modulene](https://github.com/gleam-lang/gleam/blob/master/lib/gleam/date.gleam)