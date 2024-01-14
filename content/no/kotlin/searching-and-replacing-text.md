---
title:    "Kotlin: Søking og erstattning av tekst"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger når du jobber med programmering, må du bytte ut deler av teksten din for å gjøre koden mer effektiv og lesbar. Det kan være en kjedelig og tidkrevende oppgave å gjøre dette manuelt. Derfor kommer verktøy som "søk og erstatt" godt med for å hjelpe i slike situasjoner.

## Hvordan gjøre det
Det er flere måter man kan erstatte tekst i Kotlin. Det mest vanlige er å bruke String-klassen sin innebygde `replace()`-metode. For eksempel, hvis du har en String med teksten "Hello World" og ønsker å erstatte "World" med "Kotlin", kan du gjøre følgende:

```Kotlin
var tekst = "Hello World"
tekst = tekst.replace("World", "Kotlin")
println(tekst)
```

Dette vil gi følgende output: "Hello Kotlin". Du kan også bruke `replace()`-metoden sammen med regular expressions for en mer avansert søk og erstatting.

```Kotlin
val regex = "[aeiou]".toRegex()
var tekst = "Hello World"
tekst = tekst.replace(regex, "o")
println(tekst)
```

Output vil da bli: "Hollo World", hvor alle vokaler i teksten er erstattet med bokstaven "o".

## Dypdykk
Det er mange flere måter å søke og erstatte tekst i Kotlin på, avhengig av hvilken type oppgave du har. Du kan for eksempel søke etter en spesifikk del av teksten ved å bruke `indexOf()`-metoden og deretter erstatte den delen med `replaceRange()`-metoden. Det er også mulig å gjøre flere erstatninger samtidig ved å bruke `replace()`-metoden sammen med en `Map`. Det er viktig å være oppmerksom på at Kotlin er case-sensitive, så hvis du gjør en søk og erstatting med en String som har en annen casing enn det du søker etter, vil det ikke fungere som forventet.

## Se også
- Kotlin Offisiell Dokumentasjon: https://kotlinlang.org/api/latest/jvm/stdlib/
- Søk og Erstatt i Kotlin: https://www.baeldung.com/kotlin/string-replace