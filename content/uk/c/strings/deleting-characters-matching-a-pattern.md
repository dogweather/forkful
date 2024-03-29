---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:48.477190-07:00
description: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\
  \u0432\u043E\u043B\u0456\u0432, \u0449\u043E \u0432\u0456\u0434\u043F\u043E\u0432\
  \u0456\u0434\u0430\u044E\u0442\u044C \u043F\u0435\u0432\u043D\u043E\u043C\u0443\
  \ \u0448\u0430\u0431\u043B\u043E\u043D\u0443 \u0437 \u0440\u044F\u0434\u043A\u0456\
  \u0432 \u0443 C, \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0443 \u0432\u0438\u0434\
  \u0430\u043B\u0435\u043D\u043D\u0456 \u0432\u0441\u0456\u0445 \u0432\u0438\u043F\
  \u0430\u0434\u043A\u0456\u0432 \u043F\u0435\u0432\u043D\u0438\u0445 \u0441\u0438\
  \u043C\u0432\u043E\u043B\u0456\u0432, \u044F\u043A\u0456 \u0432\u0456\u0434\u043F\
  \u043E\u0432\u0456\u0434\u0430\u044E\u0442\u044C \u0437\u0430\u0437\u0434\u0430\u043B\
  \u0435\u0433\u0456\u0434\u044C\u2026"
lastmod: '2024-03-13T22:44:50.114835-06:00'
model: gpt-4-0125-preview
summary: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\
  \u0432\u043E\u043B\u0456\u0432, \u0449\u043E \u0432\u0456\u0434\u043F\u043E\u0432\
  \u0456\u0434\u0430\u044E\u0442\u044C \u043F\u0435\u0432\u043D\u043E\u043C\u0443\
  \ \u0448\u0430\u0431\u043B\u043E\u043D\u0443 \u0437 \u0440\u044F\u0434\u043A\u0456\
  \u0432 \u0443 C, \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0443 \u0432\u0438\u0434\
  \u0430\u043B\u0435\u043D\u043D\u0456 \u0432\u0441\u0456\u0445 \u0432\u0438\u043F\
  \u0430\u0434\u043A\u0456\u0432 \u043F\u0435\u0432\u043D\u0438\u0445 \u0441\u0438\
  \u043C\u0432\u043E\u043B\u0456\u0432, \u044F\u043A\u0456 \u0432\u0456\u0434\u043F\
  \u043E\u0432\u0456\u0434\u0430\u044E\u0442\u044C \u0437\u0430\u0437\u0434\u0430\u043B\
  \u0435\u0433\u0456\u0434\u044C\u2026"
title: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\u0432\
  \u043E\u043B\u0456\u0432 \u0437\u0430 \u0437\u0440\u0430\u0437\u043A\u043E\u043C"
---

{{< edit_this_page >}}

## Що і чому?

Видалення символів, що відповідають певному шаблону з рядків у C, полягає у видаленні всіх випадків певних символів, які відповідають заздалегідь визначеним критеріям. Програмісти виконують це завдання для санітації вводу, підготовки даних до обробки або просто очищення рядків для виводу або подальшої маніпуляції, забезпечуючи, щоб дані, які обробляються, були точно такими, які потрібні для даного контексту або алгоритму.

## Як:

C не має вбудованої функції для безпосереднього видалення символів з рядка на основі шаблону, на відміну від деяких мов вищого рівня. Проте ви легко можете виконати це завдання, вручну ітеруючи по рядку та будуючи новий, який виключає небажані символи. Наприклад, давайте припустимо, що ви хочете видалити всі цифри з рядка. Ви можете зробити це так:

```c
#include <stdio.h>
#include <ctype.h>

void remove_digits(char *str) {
    char *src = str, *dst = str;
    while (*src) {
        if (!isdigit((unsigned char)*src)) {
            *dst++ = *src;
        }
        src++;
    }
    *dst = '\0';
}

int main() {
    char str[] = "C програмування 101: Основи!";
    remove_digits(str);
    printf("Результат: %s\n", str);
    return 0;
}
```

Приклад виводу:
```
Результат: C програмування : Основи!
```

Цей приклад використовує `isdigit` з `ctype.h` для ідентифікації цифр, зсуваючи неномерні символи на початок рядка та завершуючи рядок після обробки всіх символів.

## Поглиблений огляд

Представлене рішення використовує підхід з двома вказівниками в межах того самого масиву для ефективного відфільтровування небажаних символів, техніка, що є символом філософії активного управління пам'яттю в C. Цей метод є ефективним, оскільки він діє на місці, уникаючи необхідності додаткового виділення пам'яті, тим самим мінімізуючи накладні витрати.

Історично, відсутність функцій високорівневої маніпуляції з рядками в C змусила програмістів розробляти глибоке розуміння обробки рядків на рівні пам'яті, що призвело до інноваційних підходів, як вище. Хоча це має перевагу більшого контролю та ефективності, це йде з вищим ризиком помилок, таких як переповнення буферу та помилки на одиницю.

У сучасних контекстах розробки, особливо тих, що наголошують на безпеку та захист, мови, які абстрагують такі низькорівневі операції можуть бути віддані перевазі для завдань маніпуляції з рядками. Тим не менш, розуміння та використання цих технік C залишається безцінним для сценаріїв, що вимагають дрібної оптимізації продуктивності або для роботи в середовищах, де мінімалізм та швидкість C є першочерговими.
