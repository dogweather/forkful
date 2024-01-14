---
title:    "Gleam: Konvertere en dato til en streng."
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Hvorfor

Når man jobber med programmering, er det ofte nødvendig å konvertere ulike datatyper til hverandre. En vanlig oppgave er å konvertere en dato til en streng, noe som kan være nyttig for å vise datoer i et brukergrensesnitt eller lagre dem i en database. I denne bloggposten skal vi se på hvordan man kan gjøre dette i Gleam-programmeringsspråket.

# Hvordan

For å konvertere en dato til en streng i Gleam, kan man bruke funksjonen `DateTime.to_string` og angi datoen man ønsker å konvertere som et argument. Her er et eksempel på hvordan dette kan gjøres:

```gleam
let date = DateTime.new(2020, 11, 05)
let date_string = DateTime.to_string(date)
```

Når man kjører denne koden, vil `date_string` inneholde følgende verdi:

```
"05.11.2020"
```

Man kan også formatere strengen på ulike måter ved å bruke flagg i `to_string`-funksjonen. For eksempel kan man få strengen til å vise månedens navn i stedet for et tall, eller vise årstallet i tosifret format. Her er et eksempel på hvordan man kan formatere datoen:

```gleam
let date = DateTime.new(2020, 11, 05)
let date_string = DateTime.to_string(date, "MMMM d, yyyy")
```

Denne koden vil gi følgende resultat:

```
"November 5, 2020"
```

# Dypdykk

Når man jobber med datoer, er det viktig å være klar over ulike typer formater og datoformateringsstandarder som finnes. Gleam bruker standarden ISO 8601 for datoer, som er et vanlig format for å representere datoer og tider.

Man kan også bruke `DateTime.from_string`-funksjonen for å konvertere en streng til en dato. Det er også mulig å arbeide med klokkeslett og tidsforskyvning ved hjelp av `DateTime`-modulen i Gleam.

# Se også

- [Gleam offisiell dokumentasjon om datatyper](https://gleam.run/book/std-lib-dates-and-times.html)
- [ISO 8601-datoformat](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Mer om datoformat og lokaliseringsstøtte i Gleam](https://gleam.run/book/i18n.html#dates-and-times)