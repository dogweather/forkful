---
title:                "Знаходження довжини рядка"
html_title:           "Arduino: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Визначення довжини рядка - це процес відліку кількості символів у рядку. Ми робимо це для різноманітних маніпуляцій з даними, наприклад, ітерацій в циклі, порівняння рядків або їх обробки.

## Як це зробити:

```C++
#include <iostream> 
#include <string>   
using namespace std;
int main() {
   string str = "Вітаємо в C++!";
   cout << "Довжина рядка = " << str.length();
   return 0;
}
```
Виходом буде:
```C++
Довжина рядка = 13
```

## Поглиблений аналіз

**Історичний контекст:** Метод `length()` є частиною стандартної бібліотеки `string`, яка була додана у C++ у стандарті ANSI C++, 1998 року.
**Альтернативи:** Ви також можете використовувати `size()`, що є синонімом `length()`.
**Подробиці реалізації:** `length()` повертає кількість символів у рядку як тип `size_t` (беззнакове ціле).

## Дивіться також:

1. [Докладніше про рядки в C++](https://en.cppreference.com/w/cpp/string/basic_string)
2. [Стандартна бібліотека C++](http://www.cplusplus.com/reference/string/)
3. [Використання `size_t` у C++](https://www.geeksforgeeks.org/size_t-data-type-c-language/)