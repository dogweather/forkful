---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що і Чому?
Витягнення підрядка - це процес отримання певної ділянки тексту з більшої строки або рядка. Програмісти роблять це, щоб отримати потрібну інформацію з великого обсягу тексту, яка необхідна для подальшої обробки або використання.

## Як:
```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string sentence = "Це речення містить потрібний підрядок.";
    string substring = sentence.substr(22, 8);

    cout << "Отриманий підрядок:" << endl;
    cout << substring << endl;

    return 0;
}
```
**Вивід:**
```
Отриманий підрядок:
потрібний
```

## Поглиблення
Історичний контекст витягнення підрядка пов’язаний з обробкою текстових даних у мові програмування FORTRAN у 1950-60 роках. Існує також інший спосіб отримання підрядка - через ітератори у C++. Також можна використовувати регулярні вирази для витягнення підрядка з використанням бібліотеки `<regex>`.

## Дивіться також:
- [Документація для функції substr() у C++](https://www.cplusplus.com/reference/string/string/substr/)
- [Огляд регулярних виразів у C++](https://www.regular-expressions.info/cpp.html)