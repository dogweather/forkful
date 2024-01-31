---
title:                "Konvertera en sträng till gemener"
date:                  2024-01-20T17:39:00.018751-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertera en sträng till gemener"

category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Omvandling av en textsträng till gemener innebär att alla versaler i en text blir små bokstäver. Programmerare använder detta för att standardisera text för sökning, jämförelse eller för att möta användarinterface-krav.

## Hur man gör:
För att konvertera en sträng till gemener i Kotlin är det enkelt - använd `toLowerCase()`-funktionen. Här är ett exempel:

```kotlin
fun main() {
    val original = "Hej Världen!"
    val lowerCased = original.toLowerCase()
    println(lowerCased)
}
```
Utskrift skulle vara:
```
hej världen!
```

## Fördjupning
Historiskt sett har hantering av text varit centralt i programmering. Funktioner för att ändra textens casing har funnits i de flesta programmeringsspråk sedan tidigt skede. I Kotlin är `toLowerCase()` en enkel och effektiv metod, men det finns alternativ. Till exempel, `decapitalize()` kan användas för att göra endast första bokstaven i en sträng till en gemen (i vissa fall).

En viktig aspekt att notera är att `toLowerCase()` hanterar lokalisering. Det betyder att den kan konvertera bokstäver baserat på specifika språkliga regler. Ett exempel på detta är tyska där 'ß' förblir oförändrad eftersom det inte finns någon versal motsvarighet.

Kotlin ger även en version av funktionen som tar en `Locale` som argument, vilket är viktigt för språk där gemener ser olika ut beroende på region. 

Implementationsdetaljer är att när `toLowerCase()` används, skapar den en ny strängobjekt eftersom strängar i Kotlin är oföränderliga.

## Se även
- Kotlin Dokumentation om `String` klassen: [Kotlin Standard Library: String](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Oracle guide till `Locale`: [Locale-sensitive Operations](https://docs.oracle.com/javase/tutorial/i18n/locale/)
- StackOverflow diskussion om `toLowerCase()`: [When to use toLowerCase() and toUpperCase() with Locale](https://stackoverflow.com/questions/234591/when-to-use-tolowercase-and-touppercase-with-locale)
