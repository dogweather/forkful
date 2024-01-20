---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing Datoer fra Strenger i Kotlin

## Hva & Hvorfor?
Å analysere en dato fra en streng handler om å konvertere tekstformen til dato- og klokkeslettverdier. Programmerere gjør dette for å gjøre brukerens input forståelig for maskinen.

## Hvordan gjør man det: 
I Kotlin, kan vi bruke `LocalDate.parse()` funksjonen til å utføre denne oppgaven. Her er et eksempel:

```kotlin
import java.time.LocalDate

fun main() {
    val streng = "2022-02-28"
    val dato = LocalDate.parse(streng)

    println(dato)
}
```

Når du kjører dette programmet, vil utskriften være:

```kotlin
2022-02-28
```

## Dypdykk: 
(1) I fortiden, før Java 8 og Kotlin, brukte vi `SimpleDateFormat`-klassen til å analysere datoer. Den har sine egne utfordringer, mestrelevant var trådsikkerhet.

(2) Et alternativ i nåtiden er `DateTimeFormatter`-klassen i `java.time`-pakken. Den ble introdusert i Java 8 og fungerer i Kotlin.

(3) `LocalDate.parse()` bruker ISO_LOCAL_DATE som standard, men du kan også spesifisere et annet format ved å bruke `DateTimeFormatter`.

```kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy")
    val streng = "28-02-2022"
    val dato = LocalDate.parse(streng, formatter)

    println(dato)  // 2022-02-28
}
```

## Se Også: 
* Offisiell Kotlin Dokumentasjon på dato og tid: [link](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/)
* En guide til Java 8 Dato og Tid API: [link](https://www.baeldung.com/java-8-date-time-intro)