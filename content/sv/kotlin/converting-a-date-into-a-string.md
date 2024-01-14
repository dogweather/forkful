---
title:    "Kotlin: Omvandla ett datum till en sträng"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera ett datum till en sträng kan vara användbart för att visa datumet i ett visst format eller för att kommunicera med system som endast accepterar datum som strängar.

## Hur man gör det
Konverteringen kan enkelt göras med hjälp av Kotlin Standardbiblioteket. Nedan följer ett exempel där vi konverterar ett datum till en sträng med formatet "YYYY-MM-DD".

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val date = LocalDate.now()
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val dateString = date.format(formatter)
    println(dateString) // Output: 2021-08-16
}
```

## Djupdykning
Kotlin Standardbiblioteket erbjuder flera olika funktioner för att konvertera datum till strängar. Här är några av de vanligaste metoderna:

- `format`: Konverterar ett datum till en sträng med hjälp av ett angivet format.
- `toString`: Konverterar ett datum till en sträng med hjälp av standardformatet för det aktuella språket.
- `formatTo`: Konverterar ett datum till en sträng och lägger till det i en befintlig StringBuilder.
- `isoFormat`: Konverterar ett datum till en ISO-sträng som följer formatet YYYY-MM-DD.

Det är också möjligt att använda `DateTimeFormatter` för att skräddarsy konverteringen av datumet till önskat format.

## Se också
- [Kotlin Standardbiblioteket](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/) - Officiell dokumentation för Kotlin Standardbiblioteket.
- [Java Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html) - Officiell dokumentation för Java Date and Time API som används av Kotlin.
- [Kotlin Datum och Tid tutorial](https://www.callicoder.com/kotlin-date-time-tutorial/) - En praktisk guide för att arbeta med datum och tid i Kotlin.