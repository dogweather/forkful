---
title:                "Kotlin: Sammenføying av strenger"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konkatenerere strenger, eller å kombinere flere strenger til en lengre streng, er en viktig del av å programmere i Kotlin. Dette er nyttig når du ønsker å lage dynamisk tekst, som for eksempel beskjeder til brukeren basert på inndata fra programmet. Å bruke konkatenering i Kotlin er en enkel, men kraftig måte å manipulere tekst på.

## Hvordan

Det er flere måter å konkatenererere strenger i Kotlin på, men en av de enkleste er å bruke plussoperatøren (+). Dette gjør at du kan legge til to strenger sammen. La oss se på et enkelt eksempel:

```Kotlin
val fornavn = "Marius"
val etternavn = "Hansen"
val fulltNavn = fornavn + " " + etternavn
println(fulltNavn)
```

Dette vil gi følgende utskrift: `Marius Hansen`. Ved å bruke plussoperatøren, kan du også legge til tall og andre verdier til strenger, som i et eksempel:

```Kotlin
val alder = 25
val beskjed = "Jeg er " + alder + " år gammel."
println(beskjed)
```

Dette vil gi følgende utskrift: `Jeg er 25 år gammel.`

Du kan også bruke funksjonen `plus()` for å konkatenerere strenger. La oss se på et eksempel:

```Kotlin
val språk = "Kotlin"
val beskrivelse = " er et kult programmeringsspråk."
val helsetning = språk.plus(beskrivelse)
println(helsetning)
```

Dette vil gi følgende utskrift: `Kotlin er et kult programmeringsspråk.`

## Dypdykk

Når du bruker plussoperatøren i Kotlin, blir det konkatenerert strenger bak kulissene ved hjelp av `StringBuilder`-klassen. Denne klassen håndterer effektivt sammenslåingen av strenger og sørger for at det ikke oppstår unødvendige kopier av strengene. Dette gjør at det ikke påvirker ytelsen til programmet.

Du kan også bruke formateringsstreng for å konkatenerere strenger og for å bytte ut verdier i en tekst. La oss se på et eksempel:

```Kotlin
val navn = "Anders"
val beskjed = "Hei, mitt navn er %s."
val helsetning = beskjed.format(navn)
println(helsetning)
```

Dette vil gi følgende utskrift: `Hei, mitt navn er Anders.` Her erstattes `%s` med verdien av variabelen `navn`.

## Se også

- [Kotlin Dokumentasjon - Strenger og tegn](https://kotlinlang.org/docs/reference/basic-types.html#strings-and-characters)
- [Kotlin i Action - Innebygde operasjoner for strenger](https://www.manning.com/books/kotlin-in-action#builtin-operations-for-strings)
- [Offisiell nettside for Kotlin](https://kotlinlang.org/)