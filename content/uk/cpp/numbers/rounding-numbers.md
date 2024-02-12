---
title:                "Округлення чисел"
date:                  2024-01-26T03:44:02.494874-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округлення чисел"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/rounding-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?
Округлення чисел означає коригування значення до найближчого цілого або заданої точності. Розробники роблять це для спрощення, дотримання обмежень реального світу або підвищення продуктивності за рахунок відмови від зайвої точності.

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