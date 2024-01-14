---
title:                "Kotlin: Skrivande till standardfel"
simple_title:         "Skrivande till standardfel"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför
Att skriva till standardfel i Kotlin är ett viktigt verktyg för alla utvecklare. Genom att skriva till standardfel kan du notera och identifiera eventuella fel i din kod, vilket hjälper till att förbättra och felsöka dina program.

## Så här gör du
För att skriva till standardfel i Kotlin, använd följande kod:

```Kotlin
System.err.println("Detta är ett felmeddelande.") 
```

Detta kommer att skriva ut ett felmeddelande i konsolen istället för standardutmatningen. Du kan också använda `System.err` för att skriva till standardfel i try-catch-block eller i en egen funktion.

```Kotlin
try {
    //kod som kan orsaka fel
} catch (e: Exception) {
    System.err.println("Detta är ett felmeddelande: ${e.message}")
}
```

När du kör detta kommer felmeddelandet att skrivas ut i rött, vilket gör det lättare att identifiera och åtgärda eventuella problem.

## Djupdykning
Skrivning till standardfel i Kotlin är inte bara användbart för att identifiera fel, det kan också vara en effektiv metod för att logga information om körningen av ditt program. Du kan till exempel skriva ut värdet på variabler eller andra relevanta uppgifter för att få en bättre förståelse för hur din kod fungerar.

Du kan även använda bibliotek som "logback" för att hantera och skicka loggningsmeddelanden till olika källor, inklusive standardfel. Detta ger dig ännu mer kontroll över hur felmeddelanden hanteras och sparar tid när du behöver felsöka dina program.

## Se även
För mer information om hur du använder standardfel i Kotlin, se följande länkar:
- [Kotlin - Using the Standard Error Stream](https://kotlinlang.org/docs/tutorials/command-line.html#using-the-standard-error-stream)
- [Baeldung - Kotlin IO: Standard Input-Output Streams](https://www.baeldung.com/kotlin-io-standard-input-output-streams)
- [Documentation - logback](http://logback.qos.ch/documentation.html)