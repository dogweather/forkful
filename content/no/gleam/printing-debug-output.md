---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Utskrift av feilsøkingsoutput er prosessen med å vise kodeløp og variabelverdier mens programmet kjører. Dette gjøres for å hjelpe programmererne med å finne og fikse problemer i koden.

## Hvordan gjøre det:

I Gleam, kan ```io.debug``` brukes til å skrive ut feilsøkingsinformasjon. Her er et enkelt eksempel:

```Gleam
import gleam/io

fn main() {
  io.debug("Hei, feilsøking!")
}
```

Når du kjører dette programmet, vil du se følgende utskrift:

```Gleam
"Hei, feilsøking!"
```

## Dyp Dykk

Feilsøkingsutskrift har en lang historie, går tilbake til dagene med fysiske printere og perforerte kort. Alternativene til feilsøkingsutskrift inkluderer logger og formelle feilsøkingsverktøy, men noen ganger kan ingenting slå enkelheten og direktheten til utskrift for feilsøking. I Gleam kan ```io.debug``` brukes til å skrive ut hvilken som helst type som kan gjengis som en streng, og på noe miljø som implementerer Gleam's IO-modul.

## Se Også

For en mer detaljert forklaring på ```io.debug``` og dens bruk, sjekk ut [Gleam IO Module Documentation](https://hexdocs.pm/gleam_erlang/gleam/io.html). For mer om feilsøking i Gleam, ta en titt på [Gleam School](https://gleam.run/tutorials/).