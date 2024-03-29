---
date: 2024-01-20 17:47:05.572039-07:00
description: "\u0412\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u0434\u043E\
  \u0432\u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430 - \u0446\u0435 \u0437\
  \u043D\u0430\u0445\u043E\u0434\u0436\u0435\u043D\u043D\u044F \u043A\u0456\u043B\u044C\
  \u043A\u043E\u0441\u0442\u0456 \u0441\u0438\u043C\u0432\u043E\u043B\u0456\u0432\
  \ \u0443 \u043D\u044C\u043E\u043C\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435\
  \ \u0449\u043E\u0431 \u0443\u043F\u0440\u0430\u0432\u043B\u044F\u0442\u0438 \u0442\
  \u0435\u043A\u0441\u0442\u043E\u0432\u0438\u043C\u0438 \u0434\u0430\u043D\u0438\u043C\
  \u0438: \u0432\u0430\u043B\u0456\u0434\u0443\u0432\u0430\u0442\u0438 \u0432\u0432\
  \u043E\u0434, \u043E\u0431\u043C\u0435\u0436\u0443\u0432\u0430\u0442\u0438\u2026"
lastmod: '2024-03-13T22:44:49.826825-06:00'
model: gpt-4-1106-preview
summary: "\u0412\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u0434\u043E\
  \u0432\u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430 - \u0446\u0435 \u0437\
  \u043D\u0430\u0445\u043E\u0434\u0436\u0435\u043D\u043D\u044F \u043A\u0456\u043B\u044C\
  \u043A\u043E\u0441\u0442\u0456 \u0441\u0438\u043C\u0432\u043E\u043B\u0456\u0432\
  \ \u0443 \u043D\u044C\u043E\u043C\u0443. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\
  \u0456\u0441\u0442\u0438 \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435\
  \ \u0449\u043E\u0431 \u0443\u043F\u0440\u0430\u0432\u043B\u044F\u0442\u0438 \u0442\
  \u0435\u043A\u0441\u0442\u043E\u0432\u0438\u043C\u0438 \u0434\u0430\u043D\u0438\u043C\
  \u0438: \u0432\u0430\u043B\u0456\u0434\u0443\u0432\u0430\u0442\u0438 \u0432\u0432\
  \u043E\u0434, \u043E\u0431\u043C\u0435\u0436\u0443\u0432\u0430\u0442\u0438\u2026"
title: "\u0412\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u0434\u043E\u0432\
  \u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Визначення довжини рядка - це знаходження кількості символів у ньому. Програмісти роблять це щоб управляти текстовими даними: валідувати ввод, обмежувати рядки, оптимізувати пам'ять.

## Як це зробити:
```C++
#include <iostream>
#include <string>

int main() {
    std::string greeting = "Привіт, як справи?";
    std::cout << "Довжина рядка: " << greeting.length() << std::endl;
    return 0;
}
```
Выдача:
```
Довжина рядка: 19
```

## Глибше занурення:
Довжина рядка важлива з часів C і `strlen` із `<cstring>`. Але в сучасному C++, `std::string::length()` ефективніший і безпечніший через об'єктно-орієнтований підхід. Є альтернативи: `std::string::size()`, який дає той самий результат. У підрахунку не враховуються нуль-термінатори, що відмінно для UTF-8 рядків. 

## Дивіться також:
- Офіційна документація std::string: http://www.cplusplus.com/reference/string/string/
- Статья про рядки в C++: https://www.geeksforgeeks.org/stdstring-class-in-c/
