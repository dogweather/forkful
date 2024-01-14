---
title:    "Gleam: Søke og erstatte tekst"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hva er Gleam?

Gleam er et programmeringsspråk som er spesielt utviklet for å lage pålitelige, skalérbare og funksjonelle applikasjoner. Det er inspirert av både Erlang og Haskell, og har en moderne og enkel syntaks som gjør det enkelt å lage kraftig kode. I denne bloggposten skal vi se nærmere på hvordan du kan bruke Gleam til å søke og erstatte tekst i dine programmer.

## Hvorfor gjøre det?

Det er mange potensielle grunner til å engasjere seg i å søke og erstatte tekst i dine programmer. Kanskje du ønsker å gjøre endringer i en stor kodebase, eller kanskje du ønsker å utføre en spesifikk oppgave på alle forekomster av en viss tekst. Uansett grunn, kan Gleam gjøre dette enkelt og effektivt.

## Slik gjør du det

For å søke og erstatte tekst i Gleam, kan du bruke den innebygde funksjonen `String.replace` som tar to argumenter: teksten du ønsker å søke etter og teksten du ønsker å erstatte med. La oss se på et eksempel:

```
Gleam
import gleam/string
fn main() {
  let tekst = "Hei, verden!"
  let ny_tekst = String.replace("Hei", "Hallo", tekst)
  IO.println(ny_tekst) // Hallo, verden!
}
```

Her bruker vi `String.replace` til å erstatte "Hei" med "Hallo" i teksten "Hei, verden!". Den nye teksten blir da "Hallo, verden!". Dette er en enkel og effektiv måte å søke og erstatte tekst i Gleam.

## Dypdykk

Som du kanskje la merke til i eksempelet over, tar `String.replace` også et tredje argument som er teksten du ønsker å søke i. Dette gjør det mulig å erstatte tekst i en bestemt del av en string. For å søke og erstatte tekst i en string fra en bestemt posisjon, kan du bruke funksjonen `String.from(tekst, start_posisjon)` som returnerer en substring fra start_posisjon og fremover.

I tillegg kan du også bruke en regex (regular expression) som første argument i `String.replace` for å utføre mer kompliserte søk og erstatninger. Dette åpner for en rekke muligheter for å søke og erstatte tekst på en mer avansert måte.

## Se også
- Offisiell Gleam nettside: https://gleam.run/
- Offisiell Gleam GitHub repository: https://github.com/gleam-lang/gleam
- Gleam dokumentasjon: https://gleam.run/book/
- Eksempler på søke og erstatte tekst i Gleam: https://github.com/gleam-lang/gleam/blob/main/lib/core/String.gleam

Takk for at du leste denne bloggposten om å søke og erstatte tekst i Gleam. Vi håper det har vært nyttig og at du kommer til å bruke disse teknikkene i fremtidige programmeringsprosjekter. Lykke til!