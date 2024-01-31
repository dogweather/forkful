---
title:                "Testien kirjoittaminen"
date:                  2024-01-19
simple_title:         "Testien kirjoittaminen"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testaus on koodin automatisoitu tarkistus virheiden löytämiseksi. Ohjelmoijat testaavat koodia varmistaakseen, että ohjelmat toimivat suunnitellusti ja tehostavat koodin ylläpidettävyyttä.

## How to:
Kotlinissa yksikkötestit kirjoitetaan usein käyttäen JUnit-kehyksellä. Esimerkki:

```kotlin
import org.junit.Test
import org.junit.Assert.*

class ExampleUnitTest {
    @Test
    fun addition_isCorrect() {
        assertEquals(4, 2 + 2)
    }
}

// Suorita testi komennolla:
// ./gradlew test --tests ExampleUnitTest
```

Tuloste:

```
Test passed: addition_isCorrect
```

## Deep Dive
JUnit käyttöön tuli Java-maailmassa, ja se on saatavilla myös Kotlinille. Vaihtoehtoina on muitakin testauskehyksiä kuten Spek ja MockK. Yksikkötestauksessa keskitytään pieniin koodinpätkiin, kuten funktioihin, kun taas integraatiotestit varmistavat että eri osat toimivat yhteen.

## See Also
- [JUnit 5 -käyttöohje](https://junit.org/junit5/docs/current/user-guide/)
- [MockK-kehyksen kotisivu](https://mockk.io/)
- [Spek framework kotisivu](https://www.spekframework.org/)
