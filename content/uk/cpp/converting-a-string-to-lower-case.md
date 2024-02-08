---
title:                "Перетворення рядка у нижній регістр"
aliases:
- uk/cpp/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:21.773624-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/converting-a-string-to-lower-case.md"
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
