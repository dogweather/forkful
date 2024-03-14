---
date: 2024-01-20 17:38:21.773624-07:00
description: ''
lastmod: '2024-03-13T22:44:49.819301-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\
  \u044F\u0434\u043A\u0430 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\u0435\
  \u0433\u0456\u0441\u0442\u0440"
---

{{< edit_this_page >}}

## What & Why?
## Що і Чому?

Приведення рядка до нижнього регістру - це процес, в якому всі великі літери в тексті замінюються на малі. Програмісти роблять це для забезпечення єдності даних, спрощення порівнянь рядків та валідації вводу.

## How to:
## Як це зробити:

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string text = "Привіт, Світе!";
    std::transform(text.begin(), text.end(), text.begin(), 
                   [](unsigned char c) { return std::tolower(c); });
    
    std::cout << text << std::endl;  // Output: привіт, світе!
    return 0;
}
```
Пам'ятайте, що в стандартній бібліотеці C++ std::tolower працює тільки з латинським алфавітом.

## Deep Dive:
## Детальний Розбір:

Конвертація рядків у нижній регістр давно є стандартною операцією, корисною для пошуку тексту та збереження даних. Важливо відмітити, що історично, ще до Unicode, обмеження в технологіях призводили до конвертації лише англійського алфавіту. З появою Unicode ситуація змінилася.

Існують альтернативні методи, такі як boost::to_lower_copy для підтримки Unicode. Також, в налаштуваннях локалізації можна вказати мову для правильної роботи std::tolower з нестандартними символами.

Деталі реалізації важливі: std::tolower не перетворить символ, якщо він уже є у нижньому регістрі або не є літерою. Точність конвертації залежить від локалі, що встановлена в поточному середовищі.

## See Also:
## Дивіться Також:

- [C++ reference on std::tolower](https://en.cppreference.com/w/cpp/string/byte/tolower)
- [Boost String Algorithms Library](https://www.boost.org/doc/libs/release/libs/algorithm/string/)
- [Unicode Case Conversion](https://www.unicode.org/reports/tr21/tr21-5.html)
