---
title:                "C++: Пошук і заміна тексту"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Заміна тексту в програмуванні є корисним інструментом, який може суттєво полегшити роботу з текстовими даними. Вона дозволяє зберігати час та уникнути помилок при ручних замінах. 

## How To 
Існує кілька способів здійснити заміну тексту в C++. Один з них використовує функцію `replace`, яка приймає початкову строку, шуканий підрядок і строку для заміни. Приклад коду та виводу результату:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string s = "Привіт, світ!";
    string replacement = "заміна";
    string pattern = "світ";
    string output = s.replace(s.find(pattern), pattern.length(), replacement);
    cout << output << endl;
    // Вивід: Привіт, заміна!
    return 0;
}
```

Інший спосіб - це використовувати оператор заміни `regex_replace`. Цей спосіб дає більші можливості для роботи з регулярними виразами. Приклад коду та результату:

```C++
#include <iostream>
#include <regex>

using namespace std;

int main() {
    string s = "Це рядок з числами: 123456789";
    regex pattern("\\d+");
    string replacement = "aaa";
    string output = regex_replace(s, pattern, replacement);
    cout << output << endl;
    // Вивід: Це рядок з числами: aaa
    return 0;
}
```

## Deep Dive
При роботі зі заміною тексту важливо враховувати різні фактори, такі як регістр символів, розмір тексту і швидкість виконання. Крім того, існують спеціальні функції для заміни повторюваних підрядків, наприклад `replaceAll` або `regex_replace_all`.

Також процес заміни тексту може бути частиною більшої задачі. Наприклад, при створенні програм з інтерфейсом користувача, користувачі можуть запитувати заміну слів або фраз у введеному тексті. Для цього необхідно добре знати функції для роботи з регулярними виразами та маніпуляції зі строками.

## See Also

* [Строкові функції в C++](https://www.cplusplus.com/reference/string/)
* [Функції для роботи з регулярними виразами в C++](https://www.cplusplus.com/reference/regex/)