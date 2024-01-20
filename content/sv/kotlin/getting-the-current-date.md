---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att hämta det aktuella datumet handlar om att få information om det nuvarande datumet inom programmet. Detta är viktigt för att programmet ska kunna utföra tidsberoende operationer.

## Hur Gör Man:
Här är en enkel kodstump i Kotlin för att få det aktuella datumet:

```Kotlin
import java.time.LocalDate

fun main() {
    val nuvarandeDatum = LocalDate.now()
    println("Aktuella datumet är: $nuvarandeDatum")
}
```

När du kör denna kod får du en utmatning som följer:

```
Aktuella datumet är: 2022-08-18
```

(Där 2022-08-18 är dagens datum.)

## Djupdykning
*Historisk kontext:* Att få det aktuella datumet är en grundläggande uppgift som har utförts i program sedan urminnes tider. I Kodi, används `LocalDate.now()` funktionen för att hämta det aktuella datumet.

*Alternativ:* Om du behöver mer specifik information än bara det aktuella datumet, kan du använda `LocalDateTime.now()` för att hämta både datum och tid:

```Kotlin
import java.time.LocalDateTime

fun main() {
    val nu = LocalDateTime.now()
    println("Aktuella datum och tid är: $nu")
}
```

*Implementeringsdetaljer:* Funktionen `LocalDate.now()` i Kotlin använder i grunden systemets standardtidszon för att bestämma det aktuella datumet.
 
## Se Även
- Officiell Kotlin-dokumentation: [LocalDate](https://kotlinlang.org/api/latest/jvm/stdlib/java.time/-local-date/index.html) och [LocalDateTime](https://kotlinlang.org/api/latest/jvm/stdlib/java.time/-local-date-time/index.html)
- Kotlin-handbok: [Arbete med datum och tid i Kotlin](https://play.kotlinlang.org/hands-on/Working%20with%20Dates%20and%20Times/01_Introduction)
- Stack Overflow: [Hämta aktuellt datum och tid i Kotlin](https://stackoverflow.com/questions/48741298/get-current-date-time-in-kotlin)