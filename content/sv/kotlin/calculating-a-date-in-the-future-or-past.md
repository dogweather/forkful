---
title:    "Kotlin: Beräkning av ett datum i framtiden eller det förflutna."
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Varför

Att kunna räkna ut en datum i framtiden eller förflutet kan vara användbart i många situationer, som att planera resor, hålla koll på deadlines eller bara för att tillfredsställa ens nyfikenhet.

## Hur man gör det

För att räkna ut ett datum i framtiden eller förflutet i Kotlin, kan vi använda funktionen `plus` och `minus` på en `LocalDate` objekt från `java.time` biblioteket.

### Kodexempel:

```Kotlin
import java.time.LocalDate

// Räkna ut datumet 30 dagar framåt
val idag = LocalDate.now()
val omTrettioDagar = idag.plusDays(30)
println("Om 30 dagar kommer det att vara $omTrettioDagar")

// Räkna ut datumet 2 månader tillbaka
val forraManaden = idag.minusMonths(2)
println("Förra månaden var det $forraManaden")
```

### Output:
```
Om 30 dagar kommer det att vara yyyy-MM-dd
Förra månaden var det yyyy-MM-dd
```

## Deep Dive

För att förstå hur datumberäkningarna fungerar i Kotlin, är det viktigt att ha en grundläggande förståelse för datumen i Java. I Java är ett datum en kombination av år, månad och dag, representerat av klassen `LocalDate` i `java.time` biblioteket. Genom att använda funktionerna `plus` och `minus` på ett `LocalDate` objekt, kan vi ändra datumen med antalet dagar, månader eller år vi anger.

En annan viktig detalj är att `LocalDate` objektet är oföränderligt, vilket betyder att när vi tillämpar en av funktionerna `plus` eller `minus` på ett objekt, returneras ett nytt objekt och det ursprungliga objektet förblir oförändrat.

## Se Reschel

- Kotlin Dokumentation om `LocalDate`: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/index.html](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time.-local-date/index.html)
- Java Dokumentation om `LocalDate`: [https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- Java.time - Paket för kotlin.system: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.time/index.html