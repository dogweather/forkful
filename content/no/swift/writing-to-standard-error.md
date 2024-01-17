---
title:                "Skriving til standardfeil"
html_title:           "Swift: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive til standard error er en måte for programmerere å håndtere feilmeldinger eller andre viktige meldinger i Swift-koden sin. Ved å skrive ut til standard error i stedet for standard output, sørger vi for at disse meldingene blir tydelig og at feilene blir fanget opp.

## Hvordan:
Her er et eksempel på hvordan du kan skrive til standard error i Swift:

```Swift
let errorMessage = "Oisann, en feil har oppstått!"

print(errorMessage, to: &standardError)
```
Dette vil skrive ut feilmeldingen til konsollen og sørge for at den blir fanget opp som en viktig melding.

Output vil se slik ut:
```
Oisann, en feil har oppstått!
```

## Dykk dypere:
I tidligere versjoner av Swift var det vanlig å bruke print()-funksjonen for å skrive til standard error. Nå kan vi bruke en dedikert to-parameter versjon av print() som lar oss spesifisere at vi vil skrive til standard error i stedet for standard output. Dette er anbefalt i tilfelle man ønsker å skrive til både standard error og standard output.

Det er også mulig å bruke log()-funksjonen fra Swifts logging API for å skrive til standard error.

Implementasjonsdetaljer:
Når vi skriver til standard error, bruker vi standard error stream for å sende meldingen. Dette er en strøm som er atskilt fra standard output stream, som brukes til å vise resultater på konsollen.

## Se også:
- [Dokumentasjon for Swifts print()-funksjon](https://developer.apple.com/documentation/swift/1541053-print)
- [Lær mer om logging i Swift](https://developer.apple.com/documentation/os/logging)