---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:09.065420-07:00
description: "Tests schrijven is code ontwikkelen die controleert of andere code correct\
  \ functioneert. Programmeurs doen dit om bugs vroegtijdig op te sporen, te zorgen\u2026"
lastmod: '2024-03-13T22:44:50.685653-06:00'
model: gpt-4-0125-preview
summary: Tests schrijven is code ontwikkelen die controleert of andere code correct
  functioneert.
title: Tests Schrijven
weight: 36
---

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
