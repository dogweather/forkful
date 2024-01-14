---
title:                "C++: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Для чого

Писання тестів є важливою частиною процесу розробки програмного забезпечення. Вони допомагають забезпечити належну функціональність програми та виявити помилки, що можуть виникнути під час виконання. Також це є важливою складовою для забезпечення якості продукту та зменшення часу на відлагодження.

## Як

Найпоширенішими інструментами для написання тестів є фреймворки, такі як Google Test та Catch2. Давайте розглянемо приклад використання Google Test для написання тестів на функцію додавання:

```C++
#include <gtest/gtest.h>

// оголошення функції додавання
int add(int a, int b) {
    return a + b;
}

// тест для перевірки правильності додавання двох цілих чисел
TEST(AddTest, PositiveNumbers) {
    EXPECT_EQ(add(2, 3), 5);
}

// тест для перевірки правильності додавання від'ємних та додатніх чисел
TEST(AddTest, NegativeAndPositiveNumbers) {
    EXPECT_EQ(add(-5, 3), -2);
}

// повідомлення про статус тесту
int main(int argc, char** argv) {
    testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

Після компіляції та запуску програми, ми отримаємо такий результат:

```bash
[==========] Running 2 tests from 1 test suite.
[----------] Global test environment set-up.
[----------] 2 tests from AddTest
[ RUN      ] AddTest.PositiveNumbers
[       OK ] AddTest.PositiveNumbers (0 ms)
[ RUN      ] AddTest.NegativeAndPositiveNumbers
[       OK ] AddTest.NegativeAndPositiveNumbers (0 ms)
[----------] 2 tests from AddTest (0 ms total)

[----------] Global test environment tear-down
[==========] 2 tests from 1 test suite ran. (1 ms total)
[  PASSED  ] 2 tests.
```

Це означає, що обидва тести пройшли успішно. Вітаємо, ми впевнені у правильній роботі функції додавання!

## Глибоке дослідження

Існує безліч підходів та стратегій для ефективного написання тестів. Наприклад, рекомендується кожен тест перевіряти лише одну конкретну функцію або метод, щоб зробити код більш читабельним та зручним для підтримки. Також важливо писати тести для обробки стандартних випадків та використовувати різні типи вхідних даних для перевірки правильності роботи програми. Дослідження та використання різних технік тестування допоможуть забезпечити високу якість вашого програмного продукту.

## Дивіться також

- [Офіційна документація Google Test](