---
title:                "Tests Schrijven"
aliases:
- /nl/java/writing-tests.md
date:                  2024-01-28T22:13:09.065420-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tests Schrijven"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/java/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Tests schrijven is code ontwikkelen die controleert of andere code correct functioneert. Programmeurs doen dit om bugs vroegtijdig op te sporen, te zorgen dat software werkt zoals verwacht en de codekwaliteit over tijd te behouden.

## Hoe:

Laten we een eenvoudige test schrijven met JUnit, een populair testraamwerk in Java. We zullen een methode testen die twee gehele getallen optelt.

```java
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

public class CalculatorTest {

    @Test
    public void testAddition() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 moet 5 zijn");
    }
}

class Calculator {
    public int add(int a, int b) {
        return a + b;
    }
}
```

Als de methode werkt, slaagt de test zonder meldingen. Als het faalt, print JUnit een fout:

```
org.opentest4j.AssertionFailedError: 2 + 3 moet 5 zijn ==> verwacht: <5> maar was: <4>
```

## Diepere Duik

Testen was niet altijd een prioriteit voor programmeurs—het kreeg aandacht met Agile ontwikkeling en praktijken zoals Testgedreven Ontwikkeling (TDD). Alternatieven voor JUnit omvatten TestNG en Spock, elk met zijn eigen voordelen. Goede tests implementeren is een kunst; het omvat meestal het nabootsen van afhankelijkheden, het vasthouden aan testpatronen en het continu integreren van tests in het bouwproces.

## Zie Ook

- JUnit 5 Gebruikersgids: [https://junit.org/junit5/docs/current/user-guide/](https://junit.org/junit5/docs/current/user-guide/)
- Artikel over Testgedreven Ontwikkeling: [https://www.agilealliance.org/glossary/tdd/](https://www.agilealliance.org/glossary/tdd/)
- Mocking frameworks: Mockito [https://site.mockito.org/](https://site.mockito.org/)
