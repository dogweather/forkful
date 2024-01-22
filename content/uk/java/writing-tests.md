---
title:                "Написання тестів"
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Писати тести - це створення коду для перевірки бізнес-логіки вашого додатку. Тестування гарантує, що ваша програма працює правильно і захищає від майбутніх помилок при змінах.

## How to: (Як на практиці:)
```Java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

class CalculatorTest {

    @Test
    void testAddition() {
        Calculator calculator = new Calculator();
        assertEquals(5, calculator.add(2, 3), "2 + 3 must equal 5");
    }
}

class Calculator {
    int add(int a, int b) {
        return a + b;
    }
}
```
Sample Output:
```
[Test successful]
```

## Deep Dive (Занурюємось глибше):
Тести в Java почали писати з JUnit, який з’явився у вісімдесятих. Є альтернативи, такі як TestNG або Spock. Важливість тестів вбудована в Agile і TDD (Test-Driven Development). Грунтувати розроблення на тестуванні – значить мінімізувати ризики.

## See Also (Дивись також):
- [JUnit 5 User Guide](https://junit.org/junit5/docs/current/user-guide/)