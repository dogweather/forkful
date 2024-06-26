---
date: 2024-01-20 17:58:07.467950-07:00
description: "Hur g\xF6r man: F\xF6r att byta ut alla f\xF6rekomster kan du anv\xE4\
  nda `replace` med Regex."
lastmod: '2024-04-05T21:53:39.193201-06:00'
model: gpt-4-1106-preview
summary: "F\xF6r att byta ut alla f\xF6rekomster kan du anv\xE4nda `replace` med Regex."
title: "S\xF6kning och ers\xE4ttning av text"
weight: 10
---

## Hur gör man:
```kotlin
fun main() {
    val originalText = "Älskar du Kotlin så mycket som jag gör?"
    val searchText = "Kotlin"
    val replacementText = "Scala"

    val newText = originalText.replace(searchText, replacementText)

    println(newText) // Output: Älskar du Scala så mycket som jag gör?
}
```

För att byta ut alla förekomster kan du använda `replace` med Regex:
```kotlin
fun main() {
    val originalText = "Kotlin är kul, och Kotlin är kraftfullt."
    val regex = Regex("Kotlin")

    val newText = originalText.replace(regex, "Java")

    println(newText) // Output: Java är kul, och Java är kraftfullt.
}
```

## Djupdykning
Söka och ersätta har sitt ursprung i tidiga textredigerare och kommandon som `sed` i Unix. I Kotlin görs det smidigt med `replace` funktionen. Alternativ till `replace` inkluderar bibliotek som Apache Commons Lang i Java som erbjuder liknande funktionalitet.

Implementationen av `replace` i Kotlin är sömlös och hanterar unicode korrekt, till skillnad från vissa äldre system. Funktionen kan utnyttja reguljära uttryck, vilket ökar flexibiliteten när det kommer till vilka mönster som kan ersättas.

## Se även
- Kotlin Dokumentation för `replace` funktionen: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html
- Apache Commons Lang StringUtils: https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html
- Reguljära uttryck i Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/
