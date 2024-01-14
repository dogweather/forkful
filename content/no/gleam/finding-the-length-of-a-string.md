---
title:                "Gleam: Å finne lengden til en streng"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Å finne lengden på en streng er en viktig oppgave i programmering. Det kan hjelpe deg med å håndtere tekst og data på en mer effektiv måte. Det kan også være nyttig når du jobber med tekstbehandling og dataanalyse. Her skal vi se på hvordan du kan finne lengden på en streng ved hjelp av Gleam-programmeringsspråket.

## Hvordan

Først må du forstå hva en streng er. En streng er en samling av tegn, vanligvis bokstaver og tall, som danner en tekst. For å finne lengden på en streng i Gleam, kan du bruke funksjonen `count` (tell). Denne funksjonen tar inn en streng som argument og returnerer antall tegn i strengen. La oss se på et eksempel:

```Gleam
let streng = "Hei, verden!"
let lengde = String.count(streng)
```

I dette eksemplet definerer vi først en streng med teksten "Hei, verden!" og kaller deretter `count`-funksjonen for å få lengden av strengen. I dette tilfellet vil variabelen `lengde` bli satt til 13, siden det er 13 tegn i teksten.

Du kan også bruke `count`-funksjonen til å finne lengden på en streng som ikke er lagret i en variabel. For eksempel:

```Gleam
let lengde = String.count("Dette er en streng")
```

I dette tilfellet vil `lengde`-variabelen bli satt til 19.

## Dykking dypere

Det er viktig å merke seg at `count`-funksjonen returnerer antall tegn og ikke antall ord. Hvis du vil finne antall ord i en streng, må du først splitte strengen opp i en liste av ord, og deretter telle antall elementer i listen. Her er en kodebit som viser hvordan du kan gjøre det:

```Gleam
let streng = "Dette er en lang streng med flere ord"
let ordliste = String.split(streng, " ") // Splitt strengen på mellomrom
let antall_ord = List.length(ordliste) // Tell antall elementer i listen
```

I dette eksemplet deler vi først strengen opp ved hjelp av `split`-funksjonen og angir et mellomrom som skilletegn. Dette resulterer i en liste av ord i variabelen `ordliste`. Deretter kaller vi `length`-funksjonen for å få antall ord i listen, som i dette tilfellet vil være 7.

# Se også

- [Offisiell Gleam-dokumentasjon for strenger](https://gleam.run/documentation/std/string/)
- [Gleam-repositoriet på GitHub](https://github.com/gleam-lang/gleam)