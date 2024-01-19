---
title:                "Генерація випадкових чисел"
html_title:           "Java: Генерація випадкових чисел"
simple_title:         "Генерація випадкових чисел"
programming_language: "C++"
category:             "C++"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?

Генерування випадкових чисел — це процес створення чисел, які не мають очевидного шаблону. Програмісти це роблять для того, щоб симулювати непередбаченості, тестувати алгоритми, забезпечувати безпеку даних та ще багато чого.

## Як це робити:

```C++
#include <random>
#include <iostream>

int main() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> distr(1, 100);

    for(int n=0; n<10; n++)
        std::cout << distr(gen) << ' ';
    return 0;
}
```

Приклад виводу:

```
23 45 67 89 12 45 78 21 34 56 
```
Із використанням бібліотеки `<random>`, цей код генерує 10 випадкових чисел від 1 до 100.

## Глибше занурення:

### Історичний контекст:
Раніше програмісти використовували функцію `rand()`, але вона не гарантувала високої якості випадкових чисел. Нині C++ пропонує `<random>` для кращого контролю над генераторами і розподілами.

### Альтернативи:
Можна використовувати `rand()`, але такі вибори можуть мати менш передбачувані результати. Бібліотека Boost також має рішення для генерування випадкових чисел.

### Деталі імплементації:
`std::random_device` витягує випадкове число; `std::mt19937` - це генератор випадкових чисел заснований на алгоритмі Mersenne Twister; `std::uniform_int_distribution<>` - це функція, що вирівнює розподіл чисел, забезпечуючи їх випадковість.

## Дивіться також:

- [cppreference.com: `<random>`](https://en.cppreference.com/w/cpp/numeric/random)
- [cplusplus.com: `rand()`](http://www.cplusplus.com/reference/cstdlib/rand/)
- [Boost Random Number Library](https://www.boost.org/doc/libs/release/libs/random/)
  
Програмування цікаве, особливо коли ми дізнаємось нові речі. Глибоко досліджуйте і продовжуйте вчитись!