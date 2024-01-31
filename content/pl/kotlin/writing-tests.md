---
title:                "Pisanie testów"
date:                  2024-01-19
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/kotlin/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testowanie to sprawdzanie czy nasz kod robi to, co ma robić. Programiści piszą testy, żeby szybciej łapać błędy i ułatwić sobie późniejsze modyfikacje kodu.

## How to:
Kotlin współpracuje z JUnit - frameworkiem do testów. Pokażemy prosty test funkcji mnożącej:

```Kotlin
import org.junit.Assert.*
import org.junit.Test

class MathUtils {
    fun multiply(x: Int, y: Int) = x * y
}

class MathUtilsTest {
    @Test
    fun testMultiply() {
        assertEquals(4, MathUtils().multiply(2, 2))
    }
}
```

Uruchomienie testu daje wynik:
```
Test passed.
```

## Deep Dive
Testowanie w Kotlinie sięga korzeniami Javy i JUnit. Alternatywy? Spek, Kotest. Kotlin pozwala na integrację z większością narzędzi stosowanych w Javie, a także z natywnymi frameworkami, co daje wybór i elastyczność. 

## See Also
- [Oficjalna dokumentacja Kotlin Test](https://kotlinlang.org/api/latest/kotlin.test/)
- [Przewodnik po JUnit 5](https://junit.org/junit5/docs/current/user-guide/)
- [Spek Framework](https://www.spekframework.org/)
- [Kotest - Kotlin Test Framework](https://kotest.io/)
