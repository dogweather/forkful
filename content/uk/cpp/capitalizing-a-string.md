---
title:                "Збільшення регістру рядка"
html_title:           "C++: Збільшення регістру рядка"
simple_title:         "Збільшення регістру рядка"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
Чому: Перетворення рядка на великі літери може бути корисним для багатьох ситуацій, таких як виведення великих заголовків або порівняння рядків.

## How To
Для того, щоб перетворити рядок на великі літери використовується функція `toupper()` у стандартній бібліотеці `cctype`:
```C++
#include <iostream>
#include <string>
#include <cctype> // бібліотека для функції toupper()
using namespace std;

int main() {
    string s = "hello world!";
    for (int i = 0; i < s.length(); i++) {
        s[i] = toupper(s[i]); // перетворюємо кожну букву на велику літеру
    }
    cout << s << endl; // виведе "HELLO WORLD!"
    return 0;
}
```

## Deep Dive
У функції `toupper()` як аргумент приймається цілочисельне значення символу, а повертається відповідне значення символу у верхньому регістрі. Це може бути корисно, наприклад, для порівняння рядків без урахування великих та малих літер. Також, ця функція працює лише з англійськими символами, тому для обробки рядків інших мов може бути необхідно використовувати інші методи.

## See Also
Дивіться також:
- [Інформація про функцію toupper() в стандартній бібліотеці cctype](https://www.cplusplus.com/reference/cctype/toupper/)
- [Приклади використання функції toupper()](https://www.geeksforgeeks.org/toupper-function-in-cpp/)