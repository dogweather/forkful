---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

**## Hva & Hvorfor?**
Å konvertere en dato til en streng betyr å endre datatypen fra en 'Date' til en 'String' i Kotlin-programmering. Dette gjøres for å gjøre håndteringen og visningen av datoen mer håndterlig og brukervennlig.

**## Hvordan**
Du kan konvertere dato til streng i Kotlin ved hjelp av `SimpleDateFormat`-klassen. Her er et eksempel:

```kotlin
import java.text.SimpleDateFormat
import java.util.Date

fun main() {
	val dato = Date()
	val format = SimpleDateFormat("dd-MM-yyyy")
	val strengDato = format.format(dato)
	
	println(strengDato)
}
```

Når du kjører denne koden, får du utskrift som:

```
02-12-2021
```
**## Dyp Dykk**
1. Historisk kontekst: Før introduksjonen av `SimpleDateFormat` i Java, og deretter Kotlin, var det mer komplisert å konvertere datoen til streng. Du måtte bruke `Date.toString()` metode som ikke tillot tilpasset format.

2. Alternativer: Det er andre måter å konvertere en dato til en streng i Kotlin. Du kan for eksempel bruke `DateTimeFormatter` med `LocalDate`.

3. Implementasjonsdetaljer: `SimpleDateFormat` fungerer ved å ta et datomønster som en streng (for eksempel "dd-MM-yyyy"). Dette brukes til å definere ønsket utgående format.

**## Se Også**
- Stack Overflow-tråder om emnet: [link1](https://stackoverflow.com/questions/5369682/get-current-time-and-date-on-android) [link2](https://stackoverflow.com/questions/46481789/kotlin-dates)