---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva tester innebär att du skapar kod som kör din applikation och verifierar att allt fungerar som det ska. Programmerare gör detta för att hitta buggar tidigt, förbättra kodkvaliteten och säkerställa att framtida förändringar inte bryter befintlig funktionalitet.

## Hur man gör:
JUnit är ett populärt verktyg för att skriva tester i Java. Här är ett enkelt exempel:

```java
import static org.junit.Assert.*;
import org.junit.Test;

public class CalculatorTest {

    @Test
    public void testAddition() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3));
    }
}

class Calculator {
    public int add(int a, int b) {
        return a + b;
    }
}
```

Kör testet och se följande output:

```
Test passed.
```

## Djupdykning:
Testning i Java har funnits sedan de tidiga dagarna av språket. JUnit, introducerat av Erich Gamma och Kent Beck, revolutionerade enhetstestning med dess klara och enkla API. Alternativ till JUnit inkluderar TestNG och Spock. Implementationen av tester innebär ofta att man använder "assertions" för att kontrollera att koden uppfyller förväntade beteenden.

## Se även:
- [JUnit användarhandbok](https://junit.org/junit5/docs/current/user-guide/)
- [Oracle's guide till JUnit](https://docs.oracle.com/javase/8/docs/technotes/guides/test/junit.html)
- [Martin Fowler's artikel om testpyramiden](https://martinfowler.com/articles/practical-test-pyramid.html)
