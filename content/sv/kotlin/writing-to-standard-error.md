---
title:    "Kotlin: Skrivning till standard fel"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error, även kallat "stderr", kan vara en användbar teknik för programmerare att felsöka och hantera fel i sina kod. Genom att skriva till stderr istället för standard output (stdout) kan du separera ut felmeddelanden från annan utdata och identifiera vad som orsakar problemet.

## Så här gör du

För att skriva till stderr i Kotlin, kan du använda standardbibliotekets "System.err" objekt. Här är en enkel kod som skriver ut ett felmeddelande till stderr:

```Kotlin
System.err.println("Ett fel har uppstått!")
```

Om du vill skriva ut mer detaljerad information i stderr, kan du använda "e.printStackTrace()" metoden. Detta kommer att skriva ut hela stacktracet av det specifika felet som uppstod, vilket kan hjälpa dig att spåra problemet.

```Kotlin
try {
    // Kod som kan orsaka fel
} catch(e: Exception) {
    System.err.println("Ett fel har uppstått: ${e.message}")
    e.printStackTrace()
}
```

När du kör detta program, kommer du att se felmeddelandet i din terminal där du kör Kotlin-koden.

```
Ett fel har uppstått: Index 5 utanför gränserna för array
java.lang.ArrayIndexOutOfBoundsException: Index 5 utanför gränserna för array
    at MainKt.main(Main.kt:6)
```

## Djupdykning

Förutom att använda "System.err" objektet, kan du också använda "printStackTrace()" metoden från "Throwable" klassen för att skriva till stderr. Detta tillåter dig att anpassa hur felmeddelandet ser ut och inkludera annan information som kan vara användbar vid felsökning. 

En annan vanlig användning av stderr är att skriva till det från en extern process eller logga i en applikation. I båda dessa fall är det möjligt att fånga och manipulera stderr-utdata för att passa dina behov.

## Se även

- [System.err API reference](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-print-stream/err.html)
- [Throwable API reference](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.-throwable/)
- [Så här felsöker du Kotlin-kod](https://kotlinlang.org/docs/reference/exceptions.html#how-to-debug-kotlin-code)