---
title:                "Pisanie testów"
date:                  2024-01-19
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"

category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/java/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Pisanie testów to tworzenie kodu sprawdzającego poprawność innego kodu. Programiści to robią, by zapewnić jakość aplikacji, wyłapać błędy i uniknąć przyszłych problemów.

## How to: (Jak to zrobić?)
Testowanie w Javie często wykonujemy z użyciem JUnit. Poniżej znajdziesz prosty test jednostkowy oraz wynik jego działania:

```Java
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class CalculatorTest {

    @Test
    public void shouldAddTwoNumbers() {
        Calculator calculator = new Calculator();
        Assertions.assertEquals(5, calculator.add(2, 3), "2 + 3 should equal 5");
    }
}

class Calculator {
    int add(int a, int b) {
        return a + b;
    }
}
```

Po uruchomieniu testu, jeśli wszystko jest OK, output będzie wyglądał tak:

```plaintext
Test run finished after 40 ms
[         1 containers found      ]
[         0 containers skipped    ]
[         1 containers started    ]
[         1 containers successful ]
[         0 containers failed     ]
[         1 tests found           ]
[         0 tests skipped         ]
[         1 tests started         ]
[         1 tests successful      ]
[         0 tests failed          ]
```

## Deep Dive (Dogłębna analiza)
Testy jednostkowe (ang. *unit tests*) pojawiły się jako część ekstremalnego programowania (XP) w latach 90. Alternatywą jest TDD (ang. *Test-Driven Development*), gdzie najpierw pisze się testy a dopiero potem kod. Ważne jest, by test był niezależny i powtarzalny. Utrudnia to zastosowanie testów do metod z efektami ubocznymi, jak np. zapis do bazy danych.

## See Also (Zobacz również)
- [JUnit 5 User Guide](https://junit.org/junit5/docs/current/user-guide/)
- [Test-Driven Development](https://en.wikipedia.org/wiki/Test-driven_development)
- [Martin Fowler on Unit Testing](https://martinfowler.com/bliki/UnitTest.html)
