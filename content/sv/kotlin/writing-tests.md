---
title:    "Kotlin: Skriva tester"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av att utveckla en robust och pålitlig kodbas. Genom att skriva tester kan du upptäcka och fixa fel i koden innan de når produktionsfasen, vilket sparar dig tid och möjligtvis kundernas pengar. Dessutom ger det dig möjligheten att dokumentera din kod och dela med dig av dina kunskaper till andra utvecklare. 

## Hur man gör

För att skriva tester i Kotlin behöver du först importera en enhetstestramverk som heter JUnit. Här är ett exempel på hur enkel enhetstestklass skulle kunna se ut: 

```Kotlin 
import org.junit.Test
import org.junit.Assert.*

class CalculatorTest {

    @Test
    fun add() {
        val calculator = Calculator()
        val result = calculator.add(2, 3)

        assertEquals(5, result)
    }
}
```

I detta exempel skapar vi en klass som heter CalculatorTest där vi testar funktionen "add" i klassen Calculator. Vi använder JUnit:s metod "assertEquals" för att verifiera att det förväntade resultatet är lika med det faktiska resultatet från testet. 

## Djupdykning

Skriva tester handlar inte bara om att verifiera om koden fungerar som den ska, utan också om att utforska och förstå koden på en djupare nivå. Genom att skriva tester tvingas du att bryta ner din kod i mindre, mer hanterbara delar vilket kan hjälpa dig att hitta potentiella fel och förbättringsmöjligheter. Dessutom kan att skriva tester hjälpa dig att förstå beroenden mellan olika delar av din kod och hur de påverkar varandra. Detta kan förhindra fel och göra det lättare att underhålla och utveckla koden i framtiden. 

## Se även

- [JUnit dokumentation](https://junit.org/junit5/docs/current/user-guide/)
- [Kotlin Test dokumentation](https://kotlinlang.org/docs/reference/testing.html)
- [Enhetstestning - Vad är det och varför gör man det?](https://www.codingdojo.com/blog/whats-unit-testing-and-why-is-it-important)