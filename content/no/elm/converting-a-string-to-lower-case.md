---
title:                "Konvertere en streng til små bokstaver"
html_title:           "Elm: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Konvertering av en tekststreng til små bokstaver er en vanlig handling som utføres av programmører for å behandle og sammenligne tekst på en mer enhetlig måte. Dette kan være nyttig når man ønsker å sammenligne to tekster, uavhengig av om de er skrevet med store eller små bokstaver.

## Hvordan:

Elm har en innebygd funksjon som heter `String.toLower`, som kan brukes til å konvertere en tekststreng til små bokstaver. Se på følgende eksempel:

```Elm
tekst = "ELM ER AWESOME"
konvertertTekst = String.toLower tekst

-- Output:
konvertertTekst = "elm er awesome"
```

I dette tilfellet er teksten "ELM ER AWESOME" konvertert til små bokstaver ved hjelp av `String.toLower`-funksjonen. Dette gjør det enklere å sammenligne teksten med andre tekststrenger som kanskje er skrevet med ulike bruk av store og små bokstaver.

En annen måte å konvertere en tekststreng til små bokstaver på, er ved å bruke `String.map`-funksjonen. Dette involverer å gå gjennom hver enkelt bokstav i tekststrengen og forandre den til en liten bokstav ved hjelp av `Char.toLower`-funksjonen. Se på følgende eksempel:

```Elm
tekst = "ELM ER AWESOME"
konvertertTekst = String.fromList (List.map Char.toLower (String.toList tekst))

-- Output:
konvertertTekst = "elm er awesome"
```

Begge disse metodene gir det samme resultatet, men ved å bruke `String.toLower`-funksjonen kan man oppnå dette med færre linjer kode.

## Dykk dypere:

Konvertering av tekst til små bokstaver er en vanlig operasjon innenfor programmering, og er tilgjengelig i de fleste programmeringsspråk. Det gjør det lettere å håndtere tekster på en konsistent måte og å sammenligne dem uavhengig av bruken av store og små bokstaver.

I tillegg til å bruke `String.toLower`-funksjonen eller `String.map`-funksjonen, kan man også bruke `String.toUpper`-funksjonen for å konvertere en tekst til store bokstaver. 

## Se også:

- Offisiell Dokumentasjon for `String`-modulen i Elm: https://package.elm-lang.org/packages/elm/core/latest/String