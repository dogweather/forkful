---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/writing-tests.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Testaaminen tarkoittaa koodin toiminnallisuuden tarkastamista automatisoiduilla testitapauksilla. Ohjelmoijat testaavat vähentääkseen bugeja ja varmistaakseen, että ohjelmisto toimii odotetulla tavalla.

## How to:
```java
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

// Yksinkertainen luokka, jonka metodia testataan
public class Calculator {
    public int add(int a, int b) {
        return a + b;
    }
}

// Testiluokka JUnitin avulla
public class CalculatorTest {
    
    @Test
    public void testAdd() {
        Calculator calculator = new Calculator();
        // Odotettu tulos: 2, kun syötteet ovat 1 ja 1
        Assertions.assertEquals(2, calculator.add(1, 1));
    }
}
```
Kun ajat testin, tuloksen pitäisi olla läpäisty testi ilman virheitä.

## Deep Dive
Testauksen historia alkaa ohjelmoinnin alkuaikoina, ja se on kehittynyt rinnalla. Vaihtoehtoina ovat esimerkiksi TestNG, Mockito ja Spock. Toteutuksessa käytetään yksikkötestien kirjoittamiseen usein JUnit-kirjastoa, joka tarjoaa annotaatioita ja assert-metodeja testien helpottamiseksi.

## See Also
- JUnit kotisivu: [https://junit.org/](https://junit.org/)
- Mockito: [https://site.mockito.org/](https://site.mockito.org/)
- Spring Boot Test -dokumentaatio: [https://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/#boot-features-testing](https://docs.spring.io/spring-boot/docs/current/reference/htmlsingle/#boot-features-testing)
