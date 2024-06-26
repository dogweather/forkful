---
date: 2024-01-26 03:44:02.494874-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : C++ \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u043A\u0456\u043B\u044C\u043A\
  \u0430 \u0441\u043F\u043E\u0441\u043E\u0431\u0456\u0432 \u043E\u043A\u0440\u0443\
  \u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\u0441\u0435\u043B, \u0442\u0430\
  \u043A\u0438\u0445 \u044F\u043A `floor()`, `ceil()`, \u0442\u0430 `round()`."
lastmod: '2024-03-13T22:44:49.834051-06:00'
model: gpt-4-0125-preview
summary: "C++ \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u043A\u0456\u043B\u044C\
  \u043A\u0430 \u0441\u043F\u043E\u0441\u043E\u0431\u0456\u0432 \u043E\u043A\u0440\
  \u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\u0441\u0435\u043B, \u0442\
  \u0430\u043A\u0438\u0445 \u044F\u043A `floor()`, `ceil()`, \u0442\u0430 `round()`."
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u043D\u044F \u0447\u0438\u0441\
  \u0435\u043B"
weight: 13
---

## Як це зробити:
C++ пропонує кілька способів округлення чисел, таких як `floor()`, `ceil()`, та `round()`:

```C++
#include <iostream>
#include <cmath> // для функцій округлення

int main() {
    double num = 3.14;

    std::cout << "підлога: " << std::floor(num) << "\n"; // Виведення: підлога: 3
    std::cout << "стеля: " << std::ceil(num) << "\n";   // Виведення: стеля: 4
    std::cout << "округлення: " << std::round(num) << "\n"; // Виведення: округлення: 3

    // Для фіксованої точності, наприклад, округлення до двох знаків після коми:
    double precise_num = 3.146;
    double множник = 100.0;
    double округлене = std::round(precise_num * множник) / множник;

    std::cout << "округлене до двох знаків після коми: " << округлене << "\n"; // Виведення: округлене до двох знаків після коми: 3.15

    return 0;
}
```

## Поглиблений Аналіз
Перед C++11, округлення залежало від ручних методів або нестандартних бібліотек. Сьогодні `<cmath>` забезпечує надійні методи. `floor()` округляє вниз, `ceil()` округляє вгору, тоді як `round()` йде до найближчого цілого, навіть враховуючи спеціальні випадки зв'язок (випадки 0.5) шляхом округлення до парного числа.

Розуміння поведінки цих функцій є важливим; наприклад, від'ємні числа можуть вас збити з пантелику (`std::round(-2.5)` дає `-2.0`).

Альтернативи? Приведення до int після додавання 0.5 для позитивних чисел було класичним трюком, але давало помилки з від'ємними і не є типонезалежним. Бібліотеки, такі як Boost, можуть пропонувати більш тонкі підходи, тоді як розширення мови або внутрішні опції компілятора можуть оптимізувати під конкретне обладнання.

## Дивіться також
- C++ довідник для `<cmath>`: https://en.cppreference.com/w/cpp/header/cmath
- IEEE стандарт для арифметики з плаваючою точкою (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Бібліотека числових конверсій Boost: https://www.boost.org/doc/libs/release/libs/numeric/conversion/
