---
title:                "Gleam: Å få dagens dato"
simple_title:         "Å få dagens dato"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få dagens dato er en vanlig oppgave innen programmering. Dette kan være nyttig for å vise brukere når noe ble opprettet, eller for å planlegge fremtidige hendelser. I dette blogginnlegget vil vi vise deg hvordan du enkelt kan få dagens dato i Gleam-programmeringsspråket.

## Hvordan

Å få dagens dato i Gleam er enkelt og krever bare noen få linjer med kode. Først må vi importere `Time`-biblioteket:

```Gleam
import Time
```

Deretter kan vi bruke funksjonen `now` for å få dagens dato og lagre den i en variabel:

```Gleam
today = Time.now()
```

Vi kan deretter bruke funksjoner fra `Time.Date`-biblioteket for å få tilgang til ulike elementer av datoen. Her er noen eksempler:

- `today.year` - får året som en `Int`-verdi
- `today.month` - får måneden som en `Int`-verdi
- `today.day` - får dagen som en `Int`-verdi
- `today.weekday` - får ukedagen som en `Int`-verdi (0 er søndag, 1 er mandag osv.)

La oss se på et eksempel som viser dagens dato på et mer leselig format:

```Gleam
today = Time.now()
date_string = "I dag er det " ++
              today.day.toString() ++ "." ++
              today.month.toString() ++ "." ++
              today.year.toString()
```

I dette eksempelet bruker vi funksjonen `toString` for å konvertere de forskjellige datoelementene til tekst og legge dem til i en variabel som heter `date_string`. Vi bruker også `++` for å kombinere tekststrenger.

Når vi nå kjører dette, vil `date_string` inneholde teksten "I dag er det 6.10.2021", avhengig av når du leser dette innlegget.

## Deep Dive

Dersom du ønsker å lære mer om hvordan `Time`-biblioteket fungerer under overflaten, kan du ta en titt på Gleams offisielle dokumentasjon. Der finner du forklaringer og eksempler på alle funksjonene som er tilgjengelige for å håndtere datoer og klokkeslett i Gleam.

Det kan også være nyttig å vite at `today`-variabelen vi opprettet faktisk er en "struct" i Gleam. Dette betyr at den inneholder flere forskjellige variabler innenfor seg, som vi kan få tilgang til ved å bruke punktnotasjon. For eksempel:

```Gleam
my_date = Time.Date.new(day: 6, month: 10, year: 2021)
today = Time.Date.now()
new_date = today |> Time.Date.set(day: 12)
```

I dette eksempelet oppretter vi en ny struct kalt `my_date` med en spesifikk dato, bruker `now`-funksjonen for å få dagens dato og lagre den i `today`, og deretter bruker vi `set`-funksjonen for å endre dagen til 12. Dette illustrerer hvordan vi kan manipulere og arbeide med datoen på en fleksibel måte.

## Se også

- [Gleam dokumentasjon om Time-biblioteket](https://gleam.run/documentation/stdlib/time/)
- [Gleam dokumentasjon om structs](https://gleam.run/documentation/types/struct/)
- [Gleam offisiell hjemmeside](https://gleam.run/)