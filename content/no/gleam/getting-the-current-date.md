---
title:    "Gleam: Å få dagens dato"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor

Dato og tid er en viktig del av enhver applikasjon eller nettside. Enten det er for å vise når noe ble opprettet, endret eller sendt, er det alltid nyttig å ha tilgang til den nåværende datoen. Med Gleam kan du enkelt få tilgang til den nåværende datoen og bruke den til å skape en bedre brukeropplevelse for din applikasjon. Les videre for å finne ut hvordan.

## Slik gjør du det

Det første du trenger å gjøre er å importere biblioteket "datetime" som vil tillate deg å manipulere datoer og tider. Deretter kan du bruke funksjonen "now" for å få tilgang til den nåværende datoen og tidspunktet. For å gjøre det mer brukervennlig, kan du deretter formatere datoen ved hjelp av modulen "format" og angir det ønskede formatet som et argument. Her er et eksempel på hvordan koden kan se ut:

```Gleam
import datetime

let current_date = datetime.now()
let formatted_date = format(current_date, "%d %B %Y")

io.println("I dag er det ", formatted_date)
```

Kjører dette eksemplet vil gi følgende output:

```
I dag er det 23 desember 2020
```

Du kan også formatere datoen til å inkludere tidspunktet ved å bruke "%H:%M" i formatet. Dette vil da gi følgende output:

```
I dag er det 23 desember 2020, klokken 16:30
```

## Dypdykk

For mer avanserte bruksområder, kan du også bruke Gleam til å manipulere datoer og tider. Dette inkluderer å legge til eller trekke fra et spesifikk antall dager, måneder eller år fra den nåværende datoen. Du kan også bruke funksjoner som "is_before" og "is_after" for å sammenligne datoer. Her er noen eksempler på hvordan dette kan gjøres:

```Gleam
let ten_days_from_now = datetime.add_days(10, datetime.now())

let ten_days_ago = datetime.subtract_days(10, datetime.now())

let one_year_ago = datetime.subtract_years(1, datetime.now())

let is_before = datetime.is_before(ten_days_from_now, datetime.now())

let is_after = datetime.is_after(ten_days_ago, one_year_ago)
```

Dette er bare et lite utvalg av mulighetene når det kommer til å arbeide med datoer og tider i Gleam.

## Se også

- Dokumentasjon for datetime-modulen: [https://gleam.run/modules/datetime.html](https://gleam.run/modules/datetime.html)
- Eksempler på hvordan du kan bruke datoer i Gleam: [https://github.com/gleam-lang/gleam/blob/master/examples/](https://github.com/gleam-lang/gleam/blob/master/examples/)