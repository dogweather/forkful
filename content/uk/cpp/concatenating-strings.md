---
title:                "Об'єднання рядків"
html_title:           "C++: Об'єднання рядків"
simple_title:         "Об'єднання рядків"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

Злиття рядків є важливим аспектом програмування, який дозволяє об'єднати різні рядки в один. Це дозволяє створювати більш складні текстові повідомлення та динамічні рядки для використання в програмі.

## Як злити рядки в C++

Злиття рядків можна здійснити за допомогою оператора '+' або функції `concat` з бібліотеки `string`. Наприклад:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str1 = "Це";
    string str2 = " рядок.";

    string result = str1 + str2;

    cout << result; // Виводиться "Це рядок."
    return 0;
}
```

## Глибше вивчення

Під час злиття рядків, їх значення не змінюються, а створюється новий рядок, який містить комбінацію двох або більше рядків. Крім того, злиття рядків можна використовувати для об'єднання різних типів даних, наприклад, чисел і рядків.

## Див. також

- [Документація злиття рядків в C++](https://www.cplusplus.com/reference/string/string/operator+/)
- [Туторіал з роботи з рядками в C++](https://www.geeksforgeeks.org/strings-in-c-2/)