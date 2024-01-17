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

## Hva & Hvorfor?
Hva er å finne lengden av en streng? Det betyr å telle hvor mange tegn som er i en tekst. Programmører gjør dette for å håndtere og manipulere tekst på en effektiv måte.

## Hvordan:
Å finne lengden av en streng i Elm er enkelt. Bruk funksjonen "String.length" og gi strengen som argument. Se på eksempelet under for å se hvordan det fungerer:

```Elm
String.length "Hei, verden!" -- output: 12
```

Du kan også bruke denne funksjonen på en variabel som inneholder en streng, som vist i eksempelet under:

```Elm
streng = "Dette er en streng"
String.length streng -- output: 19
```

## Dykk Dypere:
Å finne lengden av en streng er et vanlig oppgave i programmering, og det er derfor mange måter å gjøre det på. Før i tiden, da datamaskiner var begrenset til å jobbe med enkeltegn samtidig, brukte man loops for å telle tegnene i en streng. Men i dag med moderne programmeringsspråk som Elm, gjør innebygde funksjoner som "String.length" jobben for oss på en mer effektiv måte.

Hvis du vil lære mer om å håndtere tekst i Elm, kan du sjekke ut dokumentasjonen for "String" modulen og se hvilke andre nyttige funksjoner som er tilgjengelige.

## Se Også:
- Elm Dokumentasjon for "String" modulen: (https://package.elm-lang.org/packages/elm/core/latest/String)
- Elm offisielle nettside: (https://elm-lang.org/)