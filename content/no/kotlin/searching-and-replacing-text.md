---
title:                "Kotlin: Søking og utskifting av tekst"
simple_title:         "Søking og utskifting av tekst"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Hvorfor

Det å søke og erstatte tekst er en viktig del av programmering, spesielt når det gjelder å automatisere prosesser og effektivisere arbeidsflyten. Enten du er en erfaren utvikler eller nybegynner, er det å mestre søk- og erstattingsfunksjonene i Kotlin en viktig ferdighet å ha.

# Hvordan gjøre det

For å søke og erstatte tekst i Kotlin, kan du bruke funksjonen `replace()` som finnes i `kotlin.String` klassen. For å erstatte all tekst som matcher et bestemt mønster med en annen tekst, bruker du følgende syntaks:

```Kotlin
val nyTekst = originalTekst.replace("mønster", "erstattendetekst")
```

La oss si at vi har en liste med navn, og vi ønsker å bytte ut alle navn som begynner med bokstaven "A" med navnet "Andrea". Her er et eksempel på hvordan vi kan gjøre dette i Kotlin:

```Kotlin
val navnListe = listOf("Amanda", "Martin", "Julie", "Anna", "Matias")

for (navn in navnListe) {
    val nyttNavn = navn.replace("^A.*".toRegex(), "Andrea")
    println(nyttNavn)
}
```

Dette vil gi følgende output:
```
Andrea
Martin
Julie
Anna
Matias
```

# Dypdykk

I tillegg til å erstatte tekst basert på et mønster, kan du også bruke mer avanserte funksjoner som `replaceAll()` og `replaceAfter()` for å gjøre mer spesifikke endringer i teksten. Du kan også bruke `replaceFirst()` for å bare erstatte den første forekomsten av et mønster.

Det er også viktig å merke seg at `replace()`-funksjonen returnerer en ny `String` og påvirker ikke den opprinnelige `String`-variabelen. Derfor er det viktig å tilordne resultatet av `replace()` til en ny variabel eller direkte bruke den i en printsetning som vist i eksempelet ovenfor.

# Se også

- [Kotlin String documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html)
- [Kotlin Regex documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/java.util.regex.-pattern/index.html)
- [Regular Expressions in Kotlin](https://www.baeldung.com/kotlin-regular-expressions)