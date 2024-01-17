---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Gleam: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Å konvertere en streng til små bokstaver er en vanlig operasjon i programmering. Dette gjør at alle bokstavene i en streng blir omgjort til små bokstaver. Dette er nyttig når man ønsker å sammenligne eller søke i tekster uten å være avhengig av forskjeller i bokstavstørrelse.

# Hvordan:
I Gleam kan du enkelt konvertere en streng til små bokstaver ved å bruke funksjonen `String.to_lower_case()`. For eksempel:

```
Gleam
// Definere en streng
let tekst = "Hei på deg!"
// Konvertere til små bokstaver
let ny_tekst = String.to_lower_case(tekst)
// Skrive ut resultatet
IO.println(ny_tekst)
```
Dette vil gi følgende utskrift:
```
hei på deg!
```
## Dykk dypere
Konvertering av strenger til små bokstaver har vært en del av programmering siden de tidlige datamaskinene. Dette var nødvendig for å kunne håndtere begrensede lagringskapasiteter og ulike tegnsett. Alternativet til å bruke funksjonen `to_lower_case()` i Gleam er å lage en egen algoritme for konvertering. Dette kan være nyttig hvis man ønsker å tilpasse konverteringen til et spesifikt formål.

## Se også
- [Gleam dokumentasjon for `String.to_lowercase()`](https://gleam.run/module/string#to_lowercase)
- [Wikipedia artikel: String casing conventions](https://en.wikipedia.org/wiki/Letter_case#Case_conventions_in_computing)