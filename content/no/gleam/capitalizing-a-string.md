---
title:                "Store bokstaver i en streng"
html_title:           "Gleam: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kapitalisere en streng kan være en nyttig funksjon når du jobber med tekstbehandling eller formatering. Ved å endre hvert første bokstav i en streng til en stor bokstav, kan du skape en mer profesjonell og leservennlig tekst. I denne artikkelen vil vi utforske hvordan du kan gjøre dette i Gleam-programmeringsspråket. 

## Hvordan gjøre det

For å kapitalisere en streng, kan du bruke funksjonen `String.capitalize` i Gleam. Denne funksjonen tar en streng som parameter og returnerer en ny streng med første bokstav stor. Her er et eksempel på hvordan vi kan bruke denne funksjonen:

```Gleam
import gleam/string

let streng = "dette er en test"
let kapitalisert_streng = String.capitalize(streng)

// kapitalisert_streng = "Dette er en test"
```

Som du kan se, blir første bokstav i strengen "dette er en test" endret til stor bokstav "D". Dette kan være nyttig når du for eksempel vil formatere en overskrift eller en tittel. 

Det er også verdt å nevne at `String.capitalize`-funksjonen kun endrer første bokstav i hver setning. Hvis du vil kapitalisere alle bokstaver i en streng, kan du bruke `String.uppercase`-funksjonen i stedet. 

## Dypdykk

I Gleam er strenger immutable, noe som betyr at de ikke kan endres etter at de er opprettet. Derfor vil `String.capitalize`-funksjonen alltid returnere en ny streng i stedet for å endre den opprinnelige. Dette er en viktig konsept i Gleam, og det er en god praksis å bruke funksjoner som returnerer nye verdier i stedet for å endre eksisterende verdier. 

Det er også verdt å merke seg at `String.capitalize` følger Unicode-standardene for å kapitalisere bokstaver. Dette betyr at den vil håndtere spesielle bokstaver og tegn som ikke finnes i det latinske alfabetet på riktig måte. 

## Se også

* [Offisiell Gleam dokumentasjon](https://gleam.run/)
* [Gleam Standardbibliotek](https://github.com/gleam-lang/gleam_stdlib)
* [Unicode standarder](https://unicode.org/)