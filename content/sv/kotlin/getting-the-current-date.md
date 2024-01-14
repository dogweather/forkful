---
title:                "Kotlin: Att få den aktuella datumet"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att få den aktuella datum och tiden är en viktig funktion i många program, särskilt när det handlar om att hantera användarsessioner, schemaläggning och datainsamling. Genom att använda Kotlin kan du enkelt implementera denna funktion och hålla dig uppdaterad om aktuella tidpunkter.

## Hur man gör

För att få den aktuella datum och tiden i Kotlin kan du använda klassen `LocalDateTime.now()`. Detta ger dig en instans av klassen LocalDateTime med den aktuella datum och tiden. För att använda detta i ditt program behöver du bara skriva:

```Kotlin
val currentTime = LocalDateTime.now()
```

Detta skapar en instans av LocalDateTime och lagrar den i variabeln `currentTime`. För att få själva datumet och tiden kan du använda metoder som `currentDate()` och `currentTime()`.

```Kotlin
// För datumet
val date = currentTime.currentDate()

// För tiden
val time = currentTime.currentTime()
```

Det finns också möjlighet att formatera datum och tid enligt dina behov, till exempel:

```Kotlin
// För att få datum i ett visst format
val formattedDate = currentTime.format(DateTimeFormatter.ofPattern("dd/MM/yyyy"))

// För att få tiden i ett annat format
val formattedTime = currentTime.format(DateTimeFormatter.ofPattern("HH:mm:ss"))
```

Resultatet av detta blir en sträng med önskad formatering, till exempel "01/10/2020" för datum och "09:00:00" för tid.

## Deep Dive

Bakom kulisserna använder Kotlin `java.time` API för att hantera datum och tid. Denna API är en del av Java 8 och ger många användbara klasser och metoder för hantering av datum och tid i ett program. Genom att använda klassen `LocalDateTime` kan du dra nytta av alla dessa funktioner och anpassa dem efter dina behov.

En annan fördel med att använda `LocalDateTime` i Kotlin är att det eliminerar behovet av manuell konvertering mellan olika tidszoner. Genom att använda `LocalDateTime.now()` får du automatiskt den aktuella tidpunkten i den lokala tidszonen.

## Se också

- [Java 8's Date and Time API](https://www.baeldung.com/java-8-date-time-intro)
- [Kotlin's LocalDateTime Class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/-local-date-time/)
- [Kotlin's Date and Time Functions](https://kotlinlang.org/docs/reference/datetime.html)