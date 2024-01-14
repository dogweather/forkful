---
title:    "Kotlin: Skrivning till standardfel"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Varför
Att skriva till standard error är ett mycket användbart verktyg för felsökning och felhantering i Kotlin-programmering. Det ger oss möjlighet att fånga och logga felmeddelanden som kan uppstå under körning av vårt program.

## Så här gör du
```Kotlin
// Kodexempel för att skriva till standard error
fun main() {
    try {
        // kod som kan orsaka fel
    } catch (e: Exception) {
        System.err.println("Ett fel har uppstått: ${e.message}")
    }
}
```

Ett enkelt sätt att skriva till standard error i Kotlin är att använda `System.err.println()` funktionen och ge den felmeddelandet som en sträng. Detta gör att felmeddelandet skrivs ut i konsollen istället för standard out, vilket ger oss möjlighet att se det även om vi inte kanske inte ser den vanliga programutskriften. Vi använder också en `try-catch` block för att fånga och behandla eventuella fel som kan uppstå.

## Djupdykning
I Kotlin kan vi också använda `System.err` objektet för att få tillgång till standard error stream direkt. Detta ger oss möjlighet att manipulera och logga felmeddelanden på olika sätt. Till exempel kan vi använda `System.err.writer()` för att få tillgång till en writer som vi kan använda för att skriva till standard error. Vi kan också ställa in en specifik `PrintStream` för att hantera felmeddelanden på ett mer anpassat sätt.

En annan användbar funktion för att hantera felmeddelanden är `System.setErr()`, vilket ger oss möjlighet att sätta en egen `PrintStream` som kommer att hantera felmeddelanden. Detta kan vara användbart om vi vill skriva felmeddelanden till en loggfil istället för den vanliga konsollen.

## Se också
- [Kotlin Standard Library Dokumentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-system/index.html) 
- [The try/catch structure in Kotlin](https://www.javatpoint.com/kotlin-try-und-katch)