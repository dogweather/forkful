---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:56:41.114779-07:00
description: "\u041A\u0430\u043A \u0441\u0434\u0435\u043B\u0430\u0442\u044C: \u0412\
  \ C \u0434\u043B\u044F \u043D\u0430\u0445\u043E\u0436\u0434\u0435\u043D\u0438\u044F\
  \ \u0434\u043B\u0438\u043D\u044B \u0441\u0442\u0440\u043E\u043A\u0438 \u043E\u0431\
  \u044B\u0447\u043D\u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\
  \u0441\u044F \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\u0430\u044F\
  \ \u0444\u0443\u043D\u043A\u0446\u0438\u044F \u0431\u0438\u0431\u043B\u0438\u043E\
  \u0442\u0435\u043A\u0438 `strlen()`. \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\
  \u0442\u043E\u0439 \u043F\u0440\u0438\u043C\u0435\u0440."
lastmod: '2024-03-13T22:44:45.899990-06:00'
model: gpt-4-0125-preview
summary: "\u0412 C \u0434\u043B\u044F \u043D\u0430\u0445\u043E\u0436\u0434\u0435\u043D\
  \u0438\u044F \u0434\u043B\u0438\u043D\u044B \u0441\u0442\u0440\u043E\u043A\u0438\
  \ \u043E\u0431\u044B\u0447\u043D\u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u0435\u0442\u0441\u044F \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043D\
  \u0430\u044F \u0444\u0443\u043D\u043A\u0446\u0438\u044F \u0431\u0438\u0431\u043B\
  \u0438\u043E\u0442\u0435\u043A\u0438 `strlen()`."
title: "\u041E\u043F\u0440\u0435\u0434\u0435\u043B\u0435\u043D\u0438\u0435 \u0434\u043B\
  \u0438\u043D\u044B \u0441\u0442\u0440\u043E\u043A\u0438"
weight: 7
---

## Как сделать:
В C для нахождения длины строки обычно используется стандартная функция библиотеки `strlen()`. Вот простой пример:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "Hello, World!";
    size_t length = strlen(myString);
    
    printf("Длина '%s' составляет %zu.\n", myString, length);
    
    return 0;
}
```

**Пример вывода:**
```
Длина 'Hello, World!' составляет 13.
```

В этом примере `strlen()` принимает строку (`myString`) в качестве входных данных и возвращает ее длину, исключая нуль-терминатор. Использование `size_t` для переменной длины рекомендуется, поскольку это беззнаковый тип целого числа, способный представлять размер самого большого возможного объекта в системе.

## Глубокое погружение:
Функция `strlen()` является частью стандартной библиотеки C с момента создания языка. Под капотом она работает, увеличивая счетчик, по мере прохождения строки до достижения нуль-терминатора. Однако, несмотря на свою простоту, необходимо учитывать производительность: поскольку `strlen()` считает символы во время выполнения, повторный вызов этой функции для одной и той же строки в цикле, например, является неэффективным.

С точки зрения безопасности, `strlen()` и другие функции обработки строк C не проверяют наличие переполнения буфера, что делает внимательное программирование важным для избежания уязвимостей. Современные альтернативы в других языках, такие как типы строк, включающие длину или использующие безопасную обработку буферов по умолчанию, устраняют некоторые из этих рисков и неэффективностей.

Несмотря на свои ограничения, понимание `strlen()` и ручной обработки строк в C критически важно для программистов, особенно при работе с низкоуровневым кодом или когда первостепенное значение имеют производительность и управление памятью. Это также предоставляет ценное понимание работы с абстракциями строк более высокого уровня в других языках.
