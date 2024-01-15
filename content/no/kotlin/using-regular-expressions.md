---
title:                "Å bruke regulære uttrykk"
html_title:           "Kotlin: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har jobbet med tekstbehandling eller dataanalyse, har du kanskje støtt på situasjoner der du trenger å søke etter spesifikke mønstre i en tekst. Dette er der regulære uttrykk kommer inn i bildet. Ved å bruke regulære uttrykk, kan du effektivt søke, erstatte og manipulere tekst ved hjelp av spesifikke mønstre.

## Slik gjør du det

Det første trinnet for å bruke regulære uttrykk i Kotlin er å importere Regex-biblioteket ved å legge til dette øverst i koden din:

```Kotlin
import kotlin.text.Regex
```

For å søke etter et mønster i en tekststreng, bruker du "find" metoden og angir det ønskede mønsteret som et regulært uttrykk. For eksempel:

```Kotlin
val tekststreng = "Jeg elsker sjokoladekake"
val pattern = Regex("sjokoladekake")
val resultat = pattern.find(tekststreng)
```

Her vil "resultat" variabelen inneholde en referanse til matchen av mønsteret "sjokoladekake" i tekststrengen. Du kan også bruke metoden "findAll" for å finne alle forekomster av mønsteret i tekststrengen.

Du kan også bruke regulære uttrykk for å erstatte tekst i en streng. For å erstatte et mønster med en ny tekst, bruker du "replace" metoden. For eksempel:

```Kotlin
val tekststreng = "Jeg ønsker meg en cupcake"
val nyTekst = Regex("cupcake")
val resultat = nyTekst.replace(tekststreng, "sjokoladekake")
```

Her vil "resultat" variabelen inneholde strengen "Jeg ønsker meg en sjokoladekake". Det er også mulig å bruke regulære uttrykk for å dele opp en streng ved hjelp av "split" metoden.

## Dykker dypere

Regulære uttrykk bruker et spesifikt syntaks for å definere mønstrene du ønsker å søke etter. Dette inkluderer spesielle tegn som representerer ulike typer tegn, som f.eks. en vilkårlig karakter eller et tall.

Enkelte vanlige regulære uttrykk som kan være nyttige i Kotlin inkluderer:

- "." som representerer en enkelt vilkårlig karakter
- "*" som representerer null eller flere forekomster av det tidligere tegnet
- "\d" som representerer en enkelt tallkarakter

I tillegg til å søke og erstatte, kan du også bruke regulære uttrykk for å validere inndata. Kotlin tilbyr en "matches" metode som sjekker om en streng samsvarer med et gitt regulært uttrykk.

Sørg for å utforske flere ressurser og eksempler for å lære mer om bruk av regulære uttrykk i Kotlin.

## Se også

- Kotlin Regex-dokumentasjon: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/
- RegExr - online regulære uttrykk tester og veiledning: https://regexr.com/