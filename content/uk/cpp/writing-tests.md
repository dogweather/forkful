---
title:                "Написання тестів"
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?

Тестування коду - це процес перевірки правильності функціонування програми. Програмісти пишуть тести, аби забезпечити надійність коду та швидке виявлення багів.

## Як це робити:

Використаємо Google Test для прикладу юніт-тестування. Спершу встановіть Google Test, а потім створіть файл тесту.

```C++
#include <gtest/gtest.h>

int Sum(int a, int b) {
    return a + b;
}

TEST(SumTest, PositiveNumbers) {
    EXPECT_EQ(7, Sum(3, 4));
}

TEST(SumTest, NegativeNumbers) {
    EXPECT_EQ(-5, Sum(-2, -3));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```
Вивід під час успішного проходження тестів:
```
[==========] Running 2 tests from 1 test case.
[----------] Global test environment set-up.
[----------] 2 tests from SumTest
[ RUN      ] SumTest.PositiveNumbers
[       OK ] SumTest.PositiveNumbers (0 ms)
[ RUN      ] SumTest.NegativeNumbers
[       OK ] SumTest.NegativeNumbers (0 ms)
[----------] 2 tests from SumTest (0 ms total)

[==========] 2 tests from 1 test case ran. (0 ms total)
[  PASSED  ] 2 tests.
```

## Поглиблений аналіз

У минулому тестування часто ігнорувалось через затратність часу, але сучасні фреймворки, такі як Google Test, CppUnit, Boost.Test, зробили процес швидшим та ефективнішим.

Є кілька видів тестування: модульне (юніт-тестування), інтеграційне, системне. Вони відрізняються об'єктом перевірки та їх метою.

При написанні тестів слід враховувати граничні випадки, залежності від зовнішніх систем, та можливість відтворення стану системи.

## Дивіться також

* Google Test офіційна документація: https://google.github.io/googletest/
* Вступ до юніт-тестування в C++ з Boost.Test: https://www.boost.org/doc/libs/release/libs/test/
* Аналіз та порівняння C++ тестових фреймворків: https://accu.org/index.php/journals/1326