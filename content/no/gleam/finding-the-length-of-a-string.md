---
title:    "Gleam: Å finne lengden til en streng"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor

Når man jobber med programmering, er det ofte nødvendig å finne lengden på en tekststreng. Dette kan være nyttig for å validere brukerinndata eller for å behandle data i en tekstbehandlingsapplikasjon. I denne bloggposten skal vi se på hvordan man kan finne lengden på en streng i Gleam-programmeringsspråket.

## Hvordan

For å finne lengden på en streng i Gleam, kan man bruke den innebygde funksjonen `String.length()`. La oss se på et eksempel

```Gleam
let string = "Gleam er et programmeringsspråk utviklet for å skrive pålitelige og effektive programmer."
let length = String.length(string)
```

I dette eksempelet har vi definert en variant ved navn `string` som inneholder teksten vi vil finne lengden på. Vi bruker deretter funksjonen `String.length()` til å finne lengden på teksten og lagre dette i en annen variant ved navn `length`. For å få ut resultatet kan vi bruke `IO.inspect()`-funksjonen og skrive til konsollen:

```Gleam
let string = "Gleam er et programmeringsspråk utviklet for å skrive pålitelige og effektive programmer."
let length = String.length(string)
IO.inspect(length) // Output: 76
```

Som vi ser, har teksten en lengde på 76 tegn.

## Dypdykk

Hvis vi tar en dypere titt på hvordan `String.length()` fungerer, ser vi at den tar inn en `String`-variant som argument og returnerer et heltall som representerer lengden til strengen. Internasjonalt har forskjellige språk forskjellige måter å telle lengden på, da noen bokstaver tar opp mer plass enn andre. I Gleam vil enkelte stadier av koden bli konvertert til en binær sekvens, og deretter vil `String.length()` beregne lengden basert på denne binære sekvensen.

## Se også

- Gleam offisiell dokumentasjon om strenger: https://gleam.run/documentation/stdlib/string/
- Enkel guide til programmering med Gleam: https://medium.com/@gleam/run-hello-world-in-gleam-506cbab70f2a
- Gleam samfunnsforum på Reddit: https://www.reddit.com/r/gleamlang/