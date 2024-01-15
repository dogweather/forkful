---
title:                "Uttrekking av substringer"
html_title:           "Kotlin: Uttrekking av substringer"
simple_title:         "Uttrekking av substringer"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor bry seg med å trekke ut delstrenger i Kotlin? Fordi det ofte er nødvendig å manipulere tekststrenger i programmeringsverdenen. Enten det er for å sortere, filtrere eller søke gjennom en streng, er det viktig å kunne kjenne til og bruke funksjoner for å ekstrahere delstrenger.

## Slik gjør du det
Det er flere måter å trekke ut delstrenger i Kotlin på, avhengig av dine behov og preferanser. Her er noen eksempler:

### Eksempel 1: Få en del av en streng
For å trekke ut en del av en streng, kan du bruke `substring()`-funksjonen. Denne funksjonen tar to parametere: startindeksen og sluttpindeksen for den delen av strengen du vil hente ut.

```Kotlin
val tekst = "Dette er en teststreng"
val delstreng = tekst.substring(5, 13)
println(delstreng) // output: er en test
```

### Eksempel 2: Få deler av en streng basert på et mønster
Hvis du vil hente ut deler av en streng basert på et spesifikt mønster, kan du bruke `split()`-funksjonen. Denne funksjonen deler opp en streng i en liste av delstrenger, basert på det spesifiserte mønsteret.

```Kotlin
val tekst = "Velkommen, dette er en tekst"
val delstrenger = tekst.split(", ")
println(delstrenger[1]) // output: dette er en tekst
```

### Eksempel 3: Bytt ut en del av en streng
Hvis du vil bytte ut en del av en streng med en annen, kan du bruke `replace()`-funksjonen. Denne funksjonen tar to parametere: den delen av strengen du vil bytte ut, og den nye delen du vil erstatte den med.

```Kotlin
val tekst = "Dette er en test"
val nyTekst = tekst.replace("test", "øvelse")
println(nyTekst) // output: Dette er en øvelse
```

## Dypdykk
For å trekke ut delstrenger i Kotlin, bruker man ofte funksjoner som `substring()`, `split()` og `replace()`. Disse funksjonene er nyttige verktøy for å manipulere tekststrenger, men det er også viktig å forstå indeksene og mønstrene bak dem for å kunne bruke dem effektivt.

## Se også
- [Kotlin dokumentasjon: String Manipulation](https://kotlinlang.org/docs/basic-types.html#string-literals)
- [Kotlin Cheat Sheet by Ray Wenderlich](https://www.raywenderlich.com/817602-kotlin-cheat-sheet-and-quick-reference)