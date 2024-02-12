---
title:                "Визначення довжини рядка"
aliases:
- uk/cpp/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:05.572039-07:00
model:                 gpt-4-1106-preview
simple_title:         "Визначення довжини рядка"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/finding-the-length-of-a-string.md"
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
