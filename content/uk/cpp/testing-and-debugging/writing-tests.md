---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:30.007176-07:00
description: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\
  \u0442\u0456\u0432 \u043D\u0430 C++ \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\
  \u0430\u0454 \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u043D\u0435\
  \u0432\u0435\u043B\u0438\u043A\u0438\u0445, \u0441\u0430\u043C\u043E\u0434\u043E\
  \u0441\u0442\u0430\u0442\u043D\u0456\u0445 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\
  , \u0449\u043E \u0430\u0432\u0442\u043E\u043C\u0430\u0442\u0438\u0447\u043D\u043E\
  \ \u043F\u0435\u0440\u0435\u0432\u0456\u0440\u044F\u044E\u0442\u044C \u043F\u043E\
  \u0432\u0435\u0434\u0456\u043D\u043A\u0443 \u0441\u0435\u043A\u0446\u0456\u0439\
  \ \u0432\u0430\u0448\u043E\u0433\u043E \u043A\u043E\u0434\u0443. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\u2026"
lastmod: '2024-03-13T22:44:49.850330-06:00'
model: gpt-4-0125-preview
summary: "\u041D\u0430\u043F\u0438\u0441\u0430\u043D\u043D\u044F \u0442\u0435\u0441\
  \u0442\u0456\u0432 \u043D\u0430 C++ \u043F\u0435\u0440\u0435\u0434\u0431\u0430\u0447\
  \u0430\u0454 \u0441\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u043D\u0435\
  \u0432\u0435\u043B\u0438\u043A\u0438\u0445, \u0441\u0430\u043C\u043E\u0434\u043E\
  \u0441\u0442\u0430\u0442\u043D\u0456\u0445 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\
  , \u0449\u043E \u0430\u0432\u0442\u043E\u043C\u0430\u0442\u0438\u0447\u043D\u043E\
  \ \u043F\u0435\u0440\u0435\u0432\u0456\u0440\u044F\u044E\u0442\u044C \u043F\u043E\
  \u0432\u0435\u0434\u0456\u043D\u043A\u0443 \u0441\u0435\u043A\u0446\u0456\u0439\
  \ \u0432\u0430\u0448\u043E\u0433\u043E \u043A\u043E\u0434\u0443. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438\u2026"
title: "\u041F\u0438\u0441\u044C\u043C\u043E \u0442\u0435\u0441\u0442\u0456\u0432"
---

{{< edit_this_page >}}

## Що і чому?

Написання тестів на C++ передбачає створення невеликих, самодостатніх програм, що автоматично перевіряють поведінку секцій вашого коду. Програмісти роблять це, щоб забезпечити роботу свого коду відповідно до очікувань, запобігти регресіям (тобто нові зміни, що ламають існуючу функціональність), та сприяти підтримці коду з плином часу.

## Як це робити:

### Використовуючи Google Test Framework

Однією з найпопулярніших сторонніх бібліотек для написання тестів на C++ є Google Test. Спочатку вам потрібно встановити Google Test і зв'язати його з вашим проектом. Після налаштування, ви можете почати писати тестові випадки.

```cpp
#include <gtest/gtest.h>

int add(int a, int b) {
    return a + b;
}

TEST(TestSuiteName, TestName) {
    EXPECT_EQ(3, add(1, 2));
}

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
```

Збережіть код у файлі та скомпілюйте його за допомогою компілятора g++, зв'язуючи бібліотеку Google Test. Якщо все налаштовано правильно, запуск результуючого виконуваного файлу запустить тест, і якщо функція `add` працює як очікується, ви побачите щось таке:

```
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from TestSuiteName
[ RUN      ] TestSuiteName.TestName
[       OK ] TestSuiteName.TestName (0 ms)
[----------] 1 test from TestSuiteName (0 ms total)

[==========] 1 test from 1 test suite ran. (1 ms total)
[  PASSED  ] 1 test.
```

### Використовуючи Catch2

Ще один популярний фреймворк для тестування на C++ - це Catch2. Він має простіший синтаксис і зазвичай не вимагає зв'язування з бібліотекою (лише з заголовочним файлом). Ось приклад того, як написати простий тест із Catch2:

```cpp
#define CATCH_CONFIG_MAIN  // Це вказує Catch надати main() - робіть це лише в одному cpp файлі
#include <catch.hpp>

int multiply(int a, int b) {
    return a * b;
}

TEST_CASE( "Цілі числа множаться", "[multiply]" ) {
    REQUIRE( multiply(2, 3) == 6 );
}
```

Після компіляції та запуску цього тесту, Catch2 надає чіткий вивід, що вказує, чи пройшов тест чи ні, разом із будь-якою інформацією, необхідною для відладки невдач:

```
===============================================================================
Всі тести пройшли (1 твердження в 1 тестовому випадку)
```

Ці приклади показують, як інтеграція фреймворків тестування у ваш робочий процес розробки на C++ може значно покращити надійність і підтримуваність вашого коду.
