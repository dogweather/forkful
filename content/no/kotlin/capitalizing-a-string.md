---
title:    "Kotlin: Store bokstaver i en tekststreng"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne formatere og endre tekst er en viktig del av programmering. I dette blogginnlegget skal vi se nærmere på hvordan du kan kapitalisere en tekststreng ved hjelp av Kotlin-programmeringsspråket.

## Hvordan

For å kapitalisere en tekststreng i Kotlin, bruker vi funksjonen `capitalize()` som er tilgjengelig for alle tekststrenger i språket. Her er et enkelt eksempel på hvordan du kan bruke denne funksjonen:

```Kotlin
val tekst = "dette er en test"
println(tekst.capitalize())
```

Dette vil skrive ut følgende:

```Kotlin
Dette er en test
```

Som du kan se, er funksjonen `capitalize()` svært enkel å bruke. Den tar den opprinnelige tekststrengen og gjør den om til en ny tekststreng med den første bokstaven i hvert ord kapitalisert. Dette gjelder uavhengig av om bokstaven var kapitalisert opprinnelig eller ikke.

Vi kan også bruke funksjonen `capitalize()` sammen med andre funksjoner for å endre tekststrengen enda mer. For eksempel kan vi bruke funksjonen `toLowerCase()` for å gjøre alle bokstavene i tekststrengen små før vi kapitaliserer den første bokstaven. Her er et eksempel på hvordan det kan se ut:

```Kotlin
val tekst = "dette er en test"
println(tekst.toLowerCase().capitalize())
```

Dette vil skrive ut følgende:

```Kotlin
Dette er en test
```

Dette er bare et eksempel på hvordan man kan bygge videre på funksjonen `capitalize()` for å få ønsket resultat.

## Dypdykk

Funksjonen `capitalize()` gjør mer enn bare å kapitalisere den første bokstaven i hvert ord. Den håndterer også akronymer og forkortelser på en smart måte. Hvis det første ordet i tekststrengen allerede er kapitalisert, vil funksjonen ikke endre det. Og hvis en forkortelse er skrevet med store bokstaver, vil funksjonen bare kapitalisere den første bokstaven og beholde resten i store bokstaver.

Et annet poeng å merke seg er at funksjonen `capitalize()` vil beholde alle tegn som ikke er bokstaver uendret. For eksempel vil den ikke endre tall eller spesialtegn, slik at du kan se at funksjonen kun endrer tekst og ikke andre data i strengen.

## Se også

- [Kotlin dokumentasjon om `capitalize()` funksjonen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Kotlin's offisielle nettside](https://kotlinlang.org/)
- [En introduksjonsartikkel til Kotlin](https://medium.com/@balamaceda_90579/introduksjon-til-kotlin-31f0801dc067)