---
title:                "Java: Att skriva tester"
simple_title:         "Att skriva tester"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/java/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester som en del av programmets utveckling kan kännas som en tråkig och onödig uppgift. Men sanningen är att tester är mycket viktiga för att säkerställa att koden fungerar som den ska och för att upptäcka eventuella fel och buggar innan de når slutanvändaren. Det sparar tid och krångel i det långa loppet och ger en bättre produkt.

## Hur du gör det

Att skriva tester i Java är inte svårt, speciellt om du redan är bekant med språket. Det finns ett inbyggt bibliotek kallat JUnit som gör det enkelt att skriva tester och köra dem. Här är ett enkelt exempel på hur du kan testa en metod som lägger ihop två tal:

```Java
public class Calculator {
    public int add(int num1, int num2) {
        return num1 + num2;
    }
}
```

```Java
import org.junit.Test;

public class CalculatorTest {
    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        assertEquals(4, calculator.add(2, 2));
    }
}
```

I det här exemplet skapar vi en klass som har en metod för att addera två tal. Sedan skapar vi en testklass som använder JUnit för att köra ett test och kontrollera att resultatet av additionen är korrekt. Genom att skriva fler tester för olika scenarier kan vi ytterligare säkerställa att metoden fungerar som den ska.

## Djupdykning

Att skriva bra tester handlar inte bara om att täcka alla delar av koden, utan också om att skriva lättlästa och strukturerade tester. Det är viktigt att ge testerna tydliga namn och att skriva dem så att de enkelt kan förstås av andra utvecklare. Det är också viktigt att testa både positiva och negativa scenarier för att säkerställa att koden hanterar felaktiga indata på rätt sätt.

En annan viktig del av att skriva tester är att fortsätta att utöka dem när koden utvecklas och uppdateras. Tester ska inte bara skrivas en gång och sedan lämnas åt sitt öde. Genom att fortsätta att förbättra och lägga till tester kan vi undvika att introducera nya fel när vi gör ändringar i koden.

## Se också

Här är några användbara länkar för att lära dig mer om att skriva tester i Java:

- [JUnit officiella hemsida](https://junit.org/junit5/)
- [Baeldung's JUnit 5 tutorial](https://www.baeldung.com/junit-5)
- [Effective Java av Joshua Bloch](https://www.amazon.com/Effective-Java-3rd-Joshua-Bloch/dp/0134685997)