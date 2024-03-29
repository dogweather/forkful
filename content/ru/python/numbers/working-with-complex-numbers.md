---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:50.390059-07:00
description: "\u041A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u044B\u0435 \u0447\
  \u0438\u0441\u043B\u0430 - \u044D\u0442\u043E \u043D\u0430\u0431\u043E\u0440 \u0447\
  \u0438\u0441\u0435\u043B \u0444\u043E\u0440\u043C\u044B `a + bi`, \u0433\u0434\u0435\
  \ `a` \u0438 `b` - \u0434\u0435\u0439\u0441\u0442\u0432\u0438\u0442\u0435\u043B\u044C\
  \u043D\u044B\u0435 \u0447\u0438\u0441\u043B\u0430, \u0430 `i` - \u043C\u043D\u0438\
  \u043C\u0430\u044F \u0435\u0434\u0438\u043D\u0438\u0446\u0430 (`i^2 = -1`). \u0412\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0440\u043E\u0432\u0430\
  \u043D\u0438\u0438 \u043C\u044B\u2026"
lastmod: '2024-03-13T22:44:44.259003-06:00'
model: gpt-4-0125-preview
summary: "\u041A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u044B\u0435 \u0447\
  \u0438\u0441\u043B\u0430 - \u044D\u0442\u043E \u043D\u0430\u0431\u043E\u0440 \u0447\
  \u0438\u0441\u0435\u043B \u0444\u043E\u0440\u043C\u044B `a + bi`, \u0433\u0434\u0435\
  \ `a` \u0438 `b` - \u0434\u0435\u0439\u0441\u0442\u0432\u0438\u0442\u0435\u043B\u044C\
  \u043D\u044B\u0435 \u0447\u0438\u0441\u043B\u0430, \u0430 `i` - \u043C\u043D\u0438\
  \u043C\u0430\u044F \u0435\u0434\u0438\u043D\u0438\u0446\u0430 (`i^2 = -1`). \u0412\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0440\u043E\u0432\u0430\
  \u043D\u0438\u0438 \u043C\u044B\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 \u043A\u043E\u043C\u043F\u043B\
  \u0435\u043A\u0441\u043D\u044B\u043C\u0438 \u0447\u0438\u0441\u043B\u0430\u043C\u0438"
---

{{< edit_this_page >}}

## Что и Почему?
Комплексные числа - это набор чисел формы `a + bi`, где `a` и `b` - действительные числа, а `i` - мнимая единица (`i^2 = -1`). В программировании мы используем их для решения задач в различных областях, таких как электротехника, обработка сигналов и квантовые вычисления.

## Как это сделать:
Python имеет встроенную поддержку комплексных чисел. Вот как вы можете с ними работать:

```Python
# Создание комплексных чисел
z = 4 + 5j
print(z)  # Вывод: (4+5j)

# Доступ к действительной и мнимой части
print(z.real)  # Вывод: 4.0
print(z.imag)  # Вывод: 5.0

# Арифметика комплексных чисел
w = 1 - 2j
print(z + w)  # Вывод: (5+3j)
print(z - w)  # Вывод: (3+7j)
print(z * w)  # Вывод: (14+2j)
print(z / w)  # Вывод: (-3.6+1.2j)

# Модуль (абсолютное значение)
print(abs(z))  # Вывод: 6.4031242374328485

# Сопряжённое комплексного числа
print(z.conjugate())  # Вывод: (4-5j)
```

## Глубокое погружение
Комплексные числа были впервые задуманы Джероламо Кардано в 16 веке. Python, среди других языков программирования, рассматривает комплексные числа как объекты первого класса. Это означает, что они встроены в язык с легкостью использования возможностей, избегая необходимости импортировать внешние библиотеки для базовых операций.

Однако для тяжёлых численных вычислений в Python есть библиотека под названием `cmath`, которая специально предназначена для комплексных чисел. Она имеет дополнительные функции, такие как `exp`, `log` и тригонометрические операции.

Когда Python недостаточно, вы можете обратиться к библиотекам, таким как NumPy, особенно для операций с массивами, включающими комплексные числа. NumPy предоставляет оптимизированные и векторизованные операции, которые критически важны для производительности в численных вычислениях.

## Смотрите также
Проверьте эти ресурсы, чтобы узнать больше:

- Официальная документация Python по комплексным числам: https://docs.python.org/3/library/stdtypes.html#typesnumeric
- Документация модуля `cmath`: https://docs.python.org/3/library/cmath.html
- NumPy для работы с массивами комплексных чисел: https://numpy.org/doc/stable/user/absolute_beginners.html#the-basics
