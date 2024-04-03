---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:30.007176-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: #."
lastmod: '2024-03-13T22:44:49.850330-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u041F\u0438\u0441\u044C\u043C\u043E \u0442\u0435\u0441\u0442\u0456\u0432"
weight: 36
---

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
