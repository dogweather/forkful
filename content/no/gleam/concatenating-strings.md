---
title:                "Sammenslåing av tekststrenger"
html_title:           "Gleam: Sammenslåing av tekststrenger"
simple_title:         "Sammenslåing av tekststrenger"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å konkatenering av strenger er en vanlig metode som brukes av programmører for å kombinere to eller flere strenger sammen til en enkelt streng.

Programmører gjør dette for å lage dynamiske uttrykk eller setninger, for eksempel å generere en personlig melding eller for å formatere tekst på en bestemt måte.

## Hvordan:

```Gleam
let navn = "Maria"
let alder = "27"

let beskjed = "Hei, mitt navn er " ++ navn ++ " og jeg er " ++ alder ++ " år gammel."

gleam_io.print(beskjed)

// Output:
// Hei, mitt navn er Maria og jeg er 27 år gammel.
```

I dette eksempelet ser vi hvordan vi kan konkatenerer to variabler, `navn` og `alder`, til en streng ved hjelp av `++`-operatøren.

## Dykk dypere

Konkatantering av strenger har vært en grunnleggende metode i programmering i lang tid. Før i tiden var det vanlig å bruke en `concat`-funksjon for å kombinere strenger, men med introduksjonen av `++`-operatøren er dette nå den foretrukne metoden.

Alternativt, i stedet for å konkatenerer strenger, kan du bruke placeholders eller variabler i en strengmal for å sette inn dynamiske verdier.

I Gleam brukes en binærtrær-implementasjon av den `++`-operatøren som sikrer rask og effektiv konkatenering av strenger.

## Se også

For mer informasjon om strenger og andre datatyper i Gleam, sjekk ut offisiell dokumentasjon her: https://gleam.run/documentation/

For en grundigere forklaring på implementasjonen av binærtrær og `++`-operatøren, kan denne artikkelen være nyttig: https://en.wikipedia.org/wiki/Balanced_binary_tree