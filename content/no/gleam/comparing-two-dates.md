---
title:                "Sammenligner to datoer"
html_title:           "Gleam: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Sammenligning av datoer er en vanlig oppgave i programmering. Enten du jobber med å sortere data eller filtrere ut bestemte tidsperioder, er det viktig å kunne sammenligne datoer på en effektiv måte. I Gleam kan du gjøre dette ved å bruke innebygde funksjoner og metoder.

## Slik gjør du det

Du kan sammenligne to datoer ved å bruke `DateTime.compare()`-funksjonen i Gleam. Denne funksjonen tar inn to datoer som argumenter og returnerer en verdi som sier om den første datoen er før, lik eller etter den andre datoen. La oss se på et eksempel:

```Gleam
import DateTime

let date1 = DateTime.from_string("2021-06-01")
let date2 = DateTime.from_string("2021-05-01")

let result = DateTime.compare(date1, date2)

pub fn compare_dates() {
  case result {
    LT -> println("Date 1 is before date 2")
    EQ -> println("Date 1 is equal to date 2")
    GT -> println("Date 1 is after date 2")
  }
}
```

I dette eksempelet bruker vi `DateTime.from_string()`-funksjonen for å konvertere strenger til datoer. Deretter sammenligner vi de to datoene ved hjelp av `DateTime.compare()`-funksjonen og skriver ut en passende melding basert på resultatet.

## Dypdykk

For å forstå hvordan Gleam sammenligner datoer, er det nyttig å vite om `FromDate`- og `ToDate`-protokollene. Disse protokollene lar oss konvertere en `DateTime`-verdi til en `Date`-verdi og vice versa. Dette betyr at vi kan bruke metoder som `compare()` til å sammenligne datoer på en enkel måte.

Et annet viktig poeng å merke seg er at `DateTime.compare()`-funksjonen også tar hensyn til tidsforskjeller når den sammenligner to datoer. Dette betyr at selv om to datoer kan virke like på overflaten, kan den ene være litt senere på dagen, og derfor bli regnet som "etter" den andre datoen.

## Se også

- [Gleam dokumentasjon for DateTime](https://gleam.run/docs/stdlib/datetime)
- [Tutorial for å jobbe med datoer og tid i Gleam](https://gleam.run/news/time)
- [GitHub repository for Gleam](https://github.com/gleam-lang/gleam)