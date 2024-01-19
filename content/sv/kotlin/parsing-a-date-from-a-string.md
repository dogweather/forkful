---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Att tolka datum från en sträng i Kotlin

## Vad & Varför?
Att tolka ett datum från en sträng innebär att omvandla läsbar text till ett riktigt datumobjekt. Programmerare gör detta för att hantera och manipulera datum och tider på ett mer exakt och enhetligt sätt.

## Hur gör man:
Kotlin erbjuder en inbyggd funktion `toLocalDate` som gör detta enkelt. Så här kan du använda den:

```kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateFormat = DateTimeFormatter.ofPattern("dd-MM-yyyy")
    val dateFromString = LocalDate.parse("18-05-2023", dateFormat)
    
    println(dateFromString)  //Output: 2023-05-18
}
```
Här kodar vi först ett datumformat som motsvarar vår strängs format. Sedan ringer vi `parse` metoden på `LocalDate` för att omvandla strängens innehåll till ett `LocalDate`-objekt.

## Djupdykning
Historiskt sett var tolkning av datum från strängar ett mer utmanande problem. Äldre språk erbjuder inte inbyggda metoder och programmeraren behövde ta hand om olika formatteringar och felhantering. 

Alternativt kan vi även använda `SimpleDateFormat` i äldre Java-kod. Dock erbjuder `LocalDate` i moderna språk som Kotlin mer läsbar och mindre buggbenägen kod.

Vad gäller implementering tar `parse` metoden en sträng och ett `DateTimeFormatter`-objekt. Den läser igenom strängen och matchar elementen i strängen med formatteringsmönstret. Ifall allt går bra, returnerar den ett fullständigt `LocalDate`-objekt.

## Se även:
1. Officiella Kotlin Dokumentationen om `LocalDate` - https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date/
2. En mer detaljerad guide om `DateTimeFormatter` - https://www.baeldung.com/kotlin-datetimeformatter
3. Diskussion om `parse` metoden på StackOverflow - https://stackoverflow.com/questions/2201925/converting-iso-8601-compliant-string-to-java-util-date