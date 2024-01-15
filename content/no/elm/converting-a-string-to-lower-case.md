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

## Hvorfor
Noen ganger når man arbeider med tekststrenger i Elm, kan det være nyttig å konvertere de til små bokstaver istedenfor store. Dette kan være for å gjøre tekstbehandling enklere eller for å sammenligne tekster uten å bekymre seg for store og små bokstaver.

## Slik gjør du det
Det er enkelt å konvertere en tekststreng til små bokstaver i Elm med funksjonen `String.toLower`. Her er et eksempel på hvordan du kan bruke den:

```Elm
text = "HEI PÅ DEG"
lowercaseText = String.toLower text
```

Det første vi gjorde var å definere en tekststreng med store bokstaver. Deretter brukte vi `String.toLower` til å konvertere den til små bokstaver og lagret resultatet i en ny variabel. Dersom vi nå skriver ut `lowercaseText`, vil vi få "hei på deg" som output.

## Dypdykk
Det kan være relevant å nevne at `String.toLower` funksjonen benytter seg av Unicode for å konvertere teksten til små bokstaver. Dette vil si at den er i stand til å håndtere ikke-ascii tegn, som for eksempel é eller ø. Det er også verdt å merke seg at denne funksjonen ikke endrer selve tekststrengen som blir gitt som argument, men returnerer en ny tekststreng med små bokstaver.

## Se også
- [String API dokumentasjon](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Unicode i Elm](https://guide.elm-lang.org/interop/unicode.html)
- [Elm programmeringsspråket (norsk)](https://elm-programming.info/)