---
title:                "Работа с комплексными числами"
date:                  2024-01-29T00:05:36.210365-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с комплексными числами"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c/working-with-complex-numbers.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Комплексные числа, сочетающие в себе действительную и мнимую части (например, 3 + 4i), ключевы в продвинутых вычислениях, таких как обработка сигналов или решение определённых уравнений. Программисты используют их для приложений, требующих интенсивных математических расчетов, где обычные числа не справляются.

## Как использовать:
С, начиная с C99, имеет встроенный комплексный тип и библиотеку. Вот как её использовать:

```C
#include <stdio.h>
#include <complex.h>

int main() {
    // Объявляем два комплексных числа
    double complex z1 = 1.0 + 3.0 * I;
    double complex z2 = 2.0 - 2.0 * I;

    // Операции с комплексными числами
    double complex sum = z1 + z2;
    double complex mult = z1 * z2;

    // Вывод результатов
    printf("Сумма: %.1f + %.1fi\n", creal(sum), cimag(sum));
    printf("Произведение: %.1f + %.1fi\n", creal(mult), cimag(mult));

    // Абсолютное значение и угол фазы
    printf("Абс(z1): %f\n", cabs(z1));
    printf("Арг(z1): %f\n", carg(z1));

    return 0;
}
```

Пример вывода:
```
Сумма: 3.0 + 1.0i
Произведение: 8.0 + 2.0i
Абс(z1): 3.162278
Арг(z1): 1.249046
```
## Подробнее
Комплексные числа имеют вековую историю, уходящую корнями в алгебру 16-го века. Вперед во времени, они теперь являются основой во многих языках программирования, не только в С.

Стандарт C99 ввёл `<complex.h>`, заголовочный файл, определяющий макросы, функции и тип данных `complex`. Существуют альтернативы - например, создание собственной структуры, но зачем изобретать велосипед? Стандартная библиотека C оптимизирована и готова к использованию.

Несмотря на свои возможности, поддержка комплексных чисел в C не лишена критики. Она может быть менее интуитивно понятной, чем аналогичные функции в языках вроде Python, а обработка крайних случаев может быть сложной. Но с точки зрения производительности это по-прежнему надёжный выбор.

## Смотри также
- Документация стандарта C99 для `<complex.h>`: https://en.cppreference.com/w/c/numeric/complex
- Стандарт IEEE для арифметики с плавающей точкой (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Онлайн-учебник по математике комплексных чисел в C: https://www.tutorialspoint.com/complex-number-arithmetic-in-c-programming