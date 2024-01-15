---
title:                "Å konvertere en streng til små bokstaver"
html_title:           "Swift: Å konvertere en streng til små bokstaver"
simple_title:         "Å konvertere en streng til små bokstaver"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver er en vanlig oppgave i programmering. Det kan være nyttig for sammenligninger, validering og formatering av data. Med Swift kan du enkelt gjøre dette med noen få linjer med kode.

## Hvordan

```Swift
let string = "HELLO WORLD"
let lowercase = string.lowercased()
print(lowercase) // output: hello world
```

Her bruker vi den innebygde funksjonen `lowercased()` på en strengvariabel for å konvertere den til små bokstaver. Det er viktig å merke seg at dette ikke endrer den originale strengen, men gir en ny streng med små bokstaver.

Hvis du ønsker å konvertere en hel streng til små bokstaver, uavhengig av om den allerede er i store eller små bokstaver, kan du bruke `caseInsensitiveCompare()`-funksjonen:

```Swift
let string = "hEllO WOrLd"
let lowercase = string.caseInsensitiveCompare("hello world")
print(lowercase) // output: true
```

Her vil `lowercase` være en Bool-verdi som indikerer om de to strengene er like, uavhengig av om de er i store eller små bokstaver.

Å konvertere en streng til små bokstaver er nyttig for å lage sammenligninger som er skriftstørrelsens uavhengige, og for å sikre at dataen er konsistent og riktig formatert.

## Dypdykk

Swift har også en rekke andre nyttige verktøy for å arbeide med strenger. For eksempel kan du bruke `capitalized`-funksjonen for å konvertere den første bokstaven i hver ord i en streng til en stor bokstav. Du kan også bruke `replacingOccurrences()`-funksjonen for å erstatte deler av strengen med en ny verdi.

Det er også mulig å utføre mer avansert manipulering av strenger ved å bruke metoder som `split()` og `joined()` for å bryte opp og kombinere deler av en streng.

Uansett hva ditt strengbehandlingsbehov er, har Swift et imponerende utvalg av verktøy for å hjelpe deg å håndtere det.

## Se også

- [Offisiell Swift dokumentasjon](https://docs.swift.org)
- [Swift String API referanse](https://developer.apple.com/documentation/swift/string)
- [Utforsking av strenger i Swift](https://www.hackingwithswift.com/articles/131/how-to-choose-between-string-interpolation-json-regular-expressions-and-more-in-swift)