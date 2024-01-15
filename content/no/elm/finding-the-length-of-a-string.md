---
title:                "Å finne lengden av en streng"
html_title:           "Elm: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å finne lengden på en streng i Elm. Kanskje trenger du å begrense inndata fra brukere, eller kanskje du vil vite hvor mange tegn som er tillatt i en bestemt tekstboks. Å finne lengden på en streng kan også være nyttig når du skal manipulere eller formatere tekster på en dynamisk måte.

## Hvordan

For å finne lengden på en streng i Elm, kan du bruke funksjonen `String.length` som tar imot en streng som argument og returnerer lengden på strengen som et heltall. For eksempel:

```Elm
String.length "Hei verden" -- returnerer 11
```

Du kan også bruke `length` funksjonen på variabler som inneholder strenger. Det er viktig å merke seg at mellomrom og spesialtegn også vil telle som et tegn i strengen.

## Dypdykk

Bak kulissene bruker `String.length` funksjonen `String.fromList` metoden for å konvertere en streng til en liste med tegn. Deretter teller den antall tegn i listen og returnerer resultatet. Du kan også bruke `List.length` metoden for å telle lengden på en liste med tegn.

En annen metode for å finne lengden på en streng er å bruke `String.split` funksjonen og deretter telle antall elementer i den resulterende listen. Dette kan være nyttig hvis du ønsker å skille en streng ved hjelp av et spesifikt tegn og deretter finne lengden på hvert element.

## Se også

- Offisiell dokumentasjon for `String.length` funksjonen i Elm: https://package.elm-lang.org/packages/elm/core/latest/String#length
- En grundig guide for å håndtere tekster i Elm: https://guide.elm-lang.org/strings/