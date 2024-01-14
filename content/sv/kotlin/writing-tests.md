---
title:                "Kotlin: Skriva tester"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## Varför

Att skriva tester är en viktig del av utvecklingen av hållbar och pålitlig kod. Genom att skriva tester kan du identifiera och lösa buggar innan de når produktion och även kontrollera att din kod fungerar som det är tänkt. Det kan även hjälpa till att förbättra din kodstruktur och göra det lättare för andra att förstå och bidra till ditt projekt.

## Hur man gör

Testning i Kotlin kan göras med hjälp av ramverket JUnit. För att skriva ett grundläggande enhetstest, kan du använda funktionen `assertEquals()` för att jämföra det förväntade resultatet med det faktiska resultatet. 

```Kotlin
@Test
fun testAddFunction() {
    val result = add(3, 5)
    assertEquals(8, result)
}
```

Du kan även använda funktionen `assertThrows()` för att kontrollera att ett visst undantag kastas. Till exempel kan du testa att en `IllegalArgumentException` kastas om du försöker dela med noll.

```Kotlin
@Test
fun testDivideByZero() {
    assertThrows<IllegalArgumentException> {
        divide(10, 0)
    }
}
```

Att skriva tester som täcker alla fall och avvikelser i din kod kan vara tidskrävande, men det är viktigt för att säkerställa att din kod fungerar korrekt. Det är också en bra praxis att skriva testerna först och sedan skriva koden för att uppfylla testfallen.

## Djupdykning

En viktig aspekt av testning är att följa principen om "Enhetstestning" där varje enskild enhet av kod testas separat. Detta hjälper till att isolera och identifiera eventuella buggar i specifika delar av koden. Det är också viktigt att skriva testfall som täcker både positiva och negativa scenarier.

Ett annat tips är att använda sig av "Test Driven Development" (TDD) där man skriver testerna först, sedan koden som uppfyller testen och sedan återgår till att förbättra testerna och koden efter behov.

## Se även

- [Kotlin Testing with JUnit](https://www.baeldung.com/kotlin/junit-5-kotlin-testing)
- [Introduction to Test Driven Development (TDD)](https://www.guru99.com/test-driven-development.html)
- [Kotlin Test Documentation](https://kotlinlang.org/docs/tutorials/testing.html)