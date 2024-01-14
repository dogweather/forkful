---
title:    "Java: Att skriva tester"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Varför?

Att skriva tester i sitt Java-programmeringsprojekt kan verka onödigt och tidskrävande, men det är faktiskt en mycket viktig del av utvecklingsprocessen. Genom att skriva tester kan du förbättra kvaliteten på din kod och minska risken för buggar och problem i framtiden.

## Så här gör du

För att skriva tester i Java behöver du först och främst använda ett testramverk, som till exempel JUnit eller TestNG. Sedan behöver du definiera testfall baserat på de funktioner som du vill testa. Låt oss ta ett enkelt exempel på en metod som adderar två tal:

```Java
public int add(int a, int b) {
    return a + b;
}
```
För att testa denna metod behöver vi definiera ett testfall som verifierar att metoden returnerar rätt värde när vi skickar in två tal som argument. Vi kan använda testramverket JUnit för att skapa detta testfall:

```Java
import static org.junit.Assert.assertEquals;

public class CalculatorTest {

    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        int result = calculator.add(5, 10);
        assertEquals(15, result);
    }
}
```

Genom att köra detta testfall kommer vi att se att metoden add() returnerar rätt värde och därmed fungerar som det är tänkt. Om vi nu skulle ändra i metoden och till exempel skriva om den till följande:

```Java
public int add(int a, int b) {
    return a - b;
}
```

Så kommer vårt testfall att misslyckas, vilket varnar oss för att något är fel med metoden. På så sätt kan vi upptäcka och åtgärda buggar tidigt i utvecklingsprocessen.

## Djupdykning

Att skriva tester är en bra vana att ha som programmerare, men det är också viktigt att förstå skillnaden mellan enhetstester och integrations-/systemtester. Enhetstester fokuserar på att testa enskilda delar av koden medan integrations-/systemtester testar hur olika delar av koden samverkar. Båda typerna av tester är viktiga och bör användas tillsammans för att uppnå en hög kvalitet på koden.

## Se också

- [En enkel guide till att skriva tester i Java](https://www.codingame.com/playgrounds/5128/test-driven-development-tdd-explained?lang=sv)
- [JUnit dokumentation](https://junit.org/junit5/docs/current/user-guide/)
- [TestNG dokumentation](https://testng.org/doc/)