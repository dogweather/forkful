---
title:                "Att skriva tester"
html_title:           "Kotlin: Att skriva tester"
simple_title:         "Att skriva tester"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva tester är en viktig del av programmering. Genom att skriva tester kan vi kolla så att koden fungerar som vi tänkt och undvika buggar och fel som kan uppstå när vi ändrar eller lägger till ny kod.

## Hur gör man:
Vi kan skriva tester i Kotlin genom att använda testramverket JUnit. Nedan följer ett enkelt kodexempel som visar hur vi kan skriva ett test som kontrollerar att en funktion returnerar rätt värde.

```Kotlin
import org.junit.Test
import org.junit.Assert

class TestClass {

    @Test
    fun testFunction() {
        // Anropa funktionen som ska testas
        val result = functionName()
        
        // Jämför resultatet med det förväntade värdet
        Assert.assertEquals(expectedValue, result)
    }
}
```

Output:
```
Test: TestClass.testFunction passed in 0.005s
```

## Djupdykning:
Historiskt sett har det funnits många olika sätt att skriva tester på, men idag är JUnit det mest använda testramverket för Java- och Kotlinbaserade system. Det finns också alternativ som TestNG, Selenium och Mockito som erbjuder lite olika funktionalitet och är mer anpassade för specifika testtyper.

När vi skriver tester är det viktigt att tänka på att det inte bara handlar om att bevisa att koden fungerar, utan också om att kontrollera att koden är läsbar och underhållbar. En väl skriven testkod kan också fungera som dokumentation för koden och gör det lättare för andra utvecklare att förstå hur den fungerar.

## Läs mer:
- [JUnit](https://junit.org/junit5/) - officiell hemsida
- [Selenium](https://www.selenium.dev/documentation/en/) - för test av webbapplikationer
- [Mockito](https://site.mockito.org/) - för att skapa "mock" objekt i tester