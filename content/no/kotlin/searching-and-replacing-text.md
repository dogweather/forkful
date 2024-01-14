---
title:                "Kotlin: Søke- og erstatningsfunksjoner for tekstbehandling"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmerere står ofte overfor en vanlig oppgave: Å søke og erstatte tekst i en kodebase. Dette kan være tidkrevende og kjedelig å gjøre manuelt, spesielt når man jobber med store og komplekse prosjekter. Derfor har Kotlin et praktisk og effektivt verktøy for å søke og erstatte tekst, som kan spare deg for masse tid og frustrasjon. Les videre for å lære hvordan du kan gjøre dette på en enkel måte.

## Slik gjør du det

Å søke og erstatte tekst i Kotlin er enkelt og kan gjøres ved å bruke funksjonen `replace()` på en String. La oss si at du har en string som ser slik ut:

```Kotlin
val navn = "Ole"
```

Hvis du vil erstatte "Ole" med "Anne", kan du gjøre følgende:

```Kotlin
val nyttNavn = navn.replace("Ole", "Anne")
println(nyttNavn)
```

Dette vil gi følgende output:

```
Anne
```

Som du ser, erstattet funksjonen `replace()` tekststrengen "Ole" med "Anne". Du kan også bruke denne funksjonen til å søke og erstatte en del av teksten, for eksempel bare de første to bokstavene:

```Kotlin
val nyttNavn = navn.replaceFirst("Ol", "An")
println(nyttNavn)
```

Output:

```
Anne
```

Hvis du ønsker å ignorere store og små bokstaver, kan du bruke `replace()` i kombinasjon med en regulær uttrykk i stedet:

```Kotlin
val nyttNavn = navn.replace(Regex("ol"), "An")
println(nyttNavn)
```

Output:

```
Anne
```

Disse er bare noen få eksempler, men funksjonen `replace()` har mange muligheter og kan tilpasses etter behov.

## Dypdykk

Funksjonen `replace()` er veldig nyttig, men hvis du ønsker å gjøre mer avanserte søk og erstatninger, kan du også bruke funksjonen `replaceEach()`. Denne funksjonen tar imot et array av tekststrenger som du vil søke etter, og et array av tekststrenger som du vil erstatte de med. Du kan også bruke dette til å søke etter regulære uttrykk og erstatte dem med noe annet.

En annen nyttig funksjon er `replaceBefore()` og `replaceAfter()`, som lar deg spesifisere en tekststreng som skal erstattes før eller etter gitt tekst eller et regluært uttrykk. Dette kan også tilpasses etter dine behov og kan gjøre søk og erstatning enda mer effektivt.

## Se også

- [Dokumentasjon for Kotlin String](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html)
- [Kotlin regex referanse](https://kotlinlang.org/docs/reference/regular-expressions.html)
- [Eksempler på søk og erstatning i Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/replace.html)