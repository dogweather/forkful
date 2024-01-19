---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Kotlin: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att beräkna ett datum i framtiden eller förflutna innebär att hitta ett specifikt datum förut eller sedan, baserat på ett givet datum. Programmerare gör detta för att hantera tidrelaterade uppgifter, som att schedulera händelser eller beräkna deadlines.

## Hur Gör Man:
Beräkning av datum kan göras enkelt i Kotlin med hjälp av java.time biblioteket. Här är ett exempel:

```Kotlin
import java.time.LocalDate
import java.time.Period

fun main () {
    val dagensDatum = LocalDate.now()
    val framtidaDatum = dagensDatum.plus(Period.ofDays(10))
    val forflutnaDatum = dagensDatum.minus(Period.ofDays(10))

    println("Dagens datum: $dagensDatum")
    println("Framtida datum: $framtidaDatum")
    println("Förflutna datum: $forflutnaDatum")
}
``` 
Kör du koden ovan kommer du få ett output som ser ut ungefär såhär:

```Kotlin
Dagens datum: 2022-02-20
Framtida datum: 2022-03-02
Förflutna datum: 2022-02-10
``` 

## Djupare Dyk:
Beräkning av datum har sina rötter i de tidiga dagarna av programmering, för att hantera tid och datumrelaterade utmaningar. Java.time biblioteket, som introducerades i Java 8, är en del av Javas svar på dessa utmaningar.

Alternativ för Kotlin inkluderar tidsbibliotek som Joda-Time och Date4J. Men java.time är inbyggt i Java, och därmed den mest direkt tillgängliga lösningen när du programmerar i Kotlin.

Vid implementation bör du vara medveten om tidzoner. Din kod kan fungera annorlunda i olika tidzoner. Testa därför alltid din kod för tidszonsberoende beteenden.

## Se Även:
1. [Java 8's nya datum och tid API (Tutorial)](https://www.baeldung.com/java-8-date-time-intro)
2. [Javadocs för java.time paket](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
3. [Kotlin officiella dokumentation](https://kotlinlang.org/docs/home.html)

Observera att det inte finns någon "Slutsats" sektion i denna artikel.