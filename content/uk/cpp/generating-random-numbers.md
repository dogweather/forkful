---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:49:09.951402-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Генерування випадкових чисел – це створення послідовності чисел, що не можна передбачити логічно. Це використовується для симуляції, ігор, безпеки та в тестуванні – скрізь, де важлива непередбачуваність.

## Як це зробити:
```C++
#include <iostream>
#include <random>

int main() {
    std::random_device rd; // створення джерела для ініціалізації
    std::mt19937 gen(rd()); // генератор Mersenne Twister
    std::uniform_int_distribution<> distrib(1, 100); // рівномірний розподіл

    for (int n=0; n<10; ++n)
        std::cout << distrib(gen) << ' '; // генерування і вивід 10 випадкових чисел
    std::cout << std::endl;

    return 0;
}
```
Очікуваний вивід: рядок із 10 випадково сгенерованих чисел від 1 до 100 (кожен із запусків дасть різний результат).

## Підводне каміння:
Раніше розповсюдженим був генератор `rand()`, але його ставлення до рівномірного розподілу та неповторності можуть бути неідеальними. Новітні стандарти C++ пропонують `<random>`, з більш якісними генераторами, такими як Mersenne Twister (MT19937). Головне – це ініціалізація генератора; `random_device` забезпечує кращу і непередбачувану початкову точку. Для різних застосувань можуть знадобитися інші розподіли — нормальний, експоненційний та інші.

## Дивіться також:
- C++11 `<random>` документація: https://en.cppreference.com/w/cpp/header/random
- C++ Working Group (WG21) папери: https://isocpp.org/std/the-standard 
- MSDN документація для генераторів випадкових чисел: https://docs.microsoft.com/en-us/cpp/standard-library/random?view=msvc-160