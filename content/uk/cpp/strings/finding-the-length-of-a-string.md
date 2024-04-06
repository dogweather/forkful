---
date: 2024-01-20 17:47:05.572039-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412\u044B\u0434\u0430\u0447\u0430."
lastmod: '2024-04-05T21:53:49.920578-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0412\u0438\u0437\u043D\u0430\u0447\u0435\u043D\u043D\u044F \u0434\u043E\u0432\
  \u0436\u0438\u043D\u0438 \u0440\u044F\u0434\u043A\u0430"
weight: 7
---

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
