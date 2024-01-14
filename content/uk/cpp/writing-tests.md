---
title:    "C++: Написання тестів"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Чому

Написання тестів є важливою частиною процесу програмування в будь-якій мові, включаючи С++. Тестування дозволяє переконатися, що код працює правильно і не має помилок. Таким чином, воно допомагає підтримувати якість програмного коду і зменшує кількість багів, що надає користь для якісного роботи програми.

## Як

Нижче наведені кілька прикладів того, як можна написати тести в мові C++ за допомогою бібліотеки Google Test. Вони створюють тести для функції, яка повертає суму двох чисел. Для кожного прикладу наведено код тесту та очікуваний результат у вигляді виведення.

```C++
// Приклад 1:
TEST(AdditionTest, PositiveNumbers) {
    int result = add(5, 3);
    EXPECT_EQ(result, 8);
}

// Вивід:
/* [----------] Global test environment set-up.
[----------] 1 test from AdditionTest
[ RUN      ] AdditionTest.PositiveNumbers
[       OK ] AdditionTest.PositiveNumbers (0 ms)
[----------] 1 test from AdditionTest (0 ms total)

[----------] Global test environment tear-down.
[==========] 1 test from 1 test case ran. (0 ms total)
[  PASSED  ] 1 test.
*/

// Приклад 2:
TEST(AdditionTest, NegativeNumbers) {
    int result = add(-5, -3);
    EXPECT_EQ(result, -8);
}

// Вивід:
/* [----------] Global test environment set-up.
[----------] 1 test from AdditionTest
[ RUN      ] AdditionTest.NegativeNumbers
[       OK ] AdditionTest.NegativeNumbers (0 ms)
[----------] 1 test from AdditionTest (0 ms total)

[----------] Global test environment tear-down.
[==========] 1 test from 1 test case ran. (0 ms total)
[  PASSED  ] 1 test.
*/

// Приклад 3:
TEST(AdditionTest, MixedNumbers) {
    int result = add(-1, 3);
    EXPECT_EQ(result, 2);
}

// Вивід:
/* [----------] Global test environment set-up.
[----------] 1 test from AdditionTest
[ RUN      ] AdditionTest.MixedNumbers
[       OK ] AdditionTest.MixedNumbers (0 ms)
[----------] 1 test from AdditionTest (0 ms total)

[----------] Global test environment tear-down.
[==========] 1 test from 1 test case ran. (0 ms total)
[  PASSED  ] 1 test.
*/
```

## Глибше погруження

Написання тестів дозволяє виявити різноманітні проблеми у вашому коді, такі як помилки в обрахунках, невідповідність типів даних та виключні ситуації. Крім того, вони забезпечують документацію для вашого коду і допомагають зрозуміти, як код повинен працювати.

Один сценарій, коли тестування особливо важливе, це під час внесення змін у вихідний код. Наприклад, якщо ви покращуєте функціонування функції, ви можете впевнитися, що нові зміни не порушують роб