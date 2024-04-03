---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:11.932403-07:00
description: 'Hoe: Kotlin gebruikt JUnit voor het testen. Hier is hoe je een eenvoudige
  test schrijft en uitvoert.'
lastmod: '2024-03-13T22:44:50.771450-06:00'
model: gpt-4-0125-preview
summary: Kotlin gebruikt JUnit voor het testen.
title: Tests Schrijven
weight: 36
---

## Hoe:
Kotlin gebruikt JUnit voor het testen. Hier is hoe je een eenvoudige test schrijft en uitvoert:

```kotlin
import org.junit.Assert.assertEquals
import org.junit.Test

class CalculatorTest {
    
    @Test
    fun `voegt twee nummers samen`() {
        assertEquals(4, Calculator.add(2, 2))
    }
}

object Calculator {
    fun add(a: Int, b: Int) = a + b
}
```

Voer het uit. Als je uitvoer er zo uitziet, zit je goed:

```
Test geslaagd
```

## Diepere Duik
JUnit, het standaard framework voor testen in Kotlin, gaat terug tot Java. Alternatieve testframeworks zijn Spek en Kotest, elk met hun eigen syntax en functies. Testen schrijven omvat vaak het begrijpen van de SUT (System Under Test) structuur, afhankelijkheden nabootsen met MockK of vergelijkbaar, en het kennen van het verschil tussen unit-, integratie-, en functionele tests.

## Zie Ook
- JUnit 5 Gebruikersgids: [junit.org/junit5/docs/current/user-guide/](https://junit.org/junit5/docs/current/user-guide/)
- MockK Bibliotheek: [mockk.io](https://mockk.io)
- Spek Framework: [spekframework.org](https://spekframework.org)
- Kotest: [kotest.io](https://kotest.io)
