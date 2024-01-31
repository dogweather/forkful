---
title:                "Написання тестів"
date:                  2024-01-19
simple_title:         "Написання тестів"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/writing-tests.md"
---

{{< edit_this_page >}}

## Що і чому?

Тестування коду — це перевірка того, чи ваші програми працюють правильно. Програмісти тестують код, щоб запобігти помилкам, гарантуючи надійність та якість програмного забезпечення.

## Як саме:

Ось приклад простого модульного тесту в C за допомогою фреймворку CUnit.

```C
#include <CUnit/Basic.h>
#include <math.h>

// Функція, яку ми тестуємо
int add(int a, int b) {
    return a + b;
}

// Тестова функція
void test_add() {
    CU_ASSERT_EQUAL(add(2, 2), 4);
}

int main() {
    // Ініціалізація тестової інфраструктури CUnit
    CU_initialize_registry();

    // Створюємо набір тестів
    CU_pSuite suite = CU_add_suite("Addition Test Suite", 0, 0);

    // Додаємо тести до набору
    CU_add_test(suite, "test of add()", test_add);

    // Виконуємо всі тести
    CU_basic_set_mode(CU_BRM_VERBOSE);
    CU_basic_run_tests();
    
    // Прибираємо за собою
    CU_cleanup_registry();
    
    return 0;
}
```

Sample output каже, що тести пройшли успішно:

```
Running suite(s): Addition Test Suite
 - test of add() ... passed
```

## Поглиблено:

Тестування в C існує давно. Спочатку воно було ручним, але з часом були розроблені автоматичні фреймворки, такі як CUnit i Check. Як альтернативи, окрім CUnit, використовують Unity та CMock для модульного тестування. Зверніть увагу на розподіл тестів у тестовому наборі і використання асертів для визначення очікуваної поведінки.

## Дивіться також:

- [CUnit - A Unit Testing Framework for C](http://cunit.sourceforge.net/)
- [Unity Test Framework](https://www.throwtheswitch.org/unity)
- [CMock - Mock/Stubs Generator for C](https://www.throwtheswitch.org/cmock)
- [Stack Overflow: Unit Testing in C](https://stackoverflow.com/questions/65820/unit-testing-in-c)
- Офіційна документація вашого компілятора C про вбудовані функції для тестування (у C11, наприклад, `<assert.h>`).
