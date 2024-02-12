---
title:                "Skriva ut felsökningsdata"
aliases:
- sv/kotlin/printing-debug-output.md
date:                  2024-01-20T17:52:52.021948-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skriva ut felsökningsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Debug-utskrifter används för att spåra vad som händer i din kod på ett enkelt sätt. Programmerare använder det för att snabbt felsöka och följa programmets flöde utan att behöva en full debugger.

## Så här gör du:
Kotlin erbjuder flera sätt att skriva ut debug-information. Här är det vanligaste:

```kotlin
fun main() {
    val myDebugMessage = "Detta är en debug-meddelande!"
    println(myDebugMessage)
    // Enklare utskrift kan se ut så här:
    println("Här sker något intressant.")

    // För mer strukturerad logging kan du använda:
    val importantValue = 42
    println("Viktigt värde är just nu: $importantValue")
}
```
Sample Output:
```
Detta är en debug-meddelande!
Här sker något intressant.
Viktigt värde är just nu: 42
```

## Fördjupning
Print-debugging är gammalt som gatan, det förlitar sig på det mänskliga ögat för att upptäcka avvikelser i konsolloggar. Det är inte det mest effektiva sättet att debugga, men det är enkelt och direkt. Alternativ inkluderar logger-bibliotek som `log4j` eller `SLF4J` som erbjuder olika loggnivåer (ERROR, WARN, INFO, DEBUG, TRACE) för mer nyanserad kontroll. När det gäller implementation så är `println` en funktion som skriver ut text till standard output (oftast terminalen) och kan användas var som helst i din Kotlin-kod.

## Se också
- Kotlin officiella dokumentation: [kotlinlang.org](https://kotlinlang.org/docs/home.html)
- Introduktion till SLF4J för Kotlin: [http://www.slf4j.org/manual.html](http://www.slf4j.org/manual.html)
- Log4j Kotlin API dokumentation: [https://logging.apache.org/log4j/kotlin/](https://logging.apache.org/log4j/kotlin/)
