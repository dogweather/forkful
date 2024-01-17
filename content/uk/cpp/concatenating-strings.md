---
title:                "З'єднання рядків"
html_title:           "C++: З'єднання рядків"
simple_title:         "З'єднання рядків"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що і чому?
Конкатенація рядків - це процес об'єднання декількох рядків в один. Програмісти часто використовують цей функціонал для створення складних повідомлень або шаблонів.

## Як:
Щоб зрозуміти, як працює конкатенація рядків, подивіться на наступний приклад, де два рядки об'єднуються за допомогою оператора "+".

```C++
#include <iostream>

using namespace std;

int main() {

    string first = "Привіт";
    string second = "Світ!";
    string concatenated = first + second;

    cout << concatenated << endl;

    return 0;
}
```

Вивід: ПривітСвіт!

Цей код використовує змінні типу string та оператор "+" для об'єднання рядків. Можна також використовувати функцію strcat () з бібліотеки string.h для з'єднання рядків.

## Глибші деталі:
Конкатенація рядків була однією з основних операцій рядків, яку багато мов програмування зараховують до свого набору функцій. Крім оператора "+", часто використовуються також функції concat () та join (). Вони є альтернативами функції strcat (), але різняться у використанні та синтаксисі.

## Дивись також:
Детальнішу інформацію про конкатенацію рядків та інші корисні функції можна знайти на наступних ресурсах:
- [https://uk.wikipedia.org/wiki/%D0%9A%D0%BE%D0%BD%D0%BA%D0%B0%D1%82%D0%B5%D0%BD%D0%B0%D1%86%D1%96%D1%8F_(%D1%80%D0%B5%D0%B7%D1%8C%D0%BA%D0%B)\_(%D0%B1%D0%B5%D0%B7\_%D0%B7%D1%81%D1%83%D0%B2%D1%83]](https://uk.wikipedia.org/wiki/%D0%9A%D0%BE%D0%BD%D0%BA%D0%B0%D1%82%D0%B5%D0%BD%D0%B0%D1%86%D1%96%D1%8F_(%D1%80%D0%B5%D0%B7%D1%8C%D0%BA%D0)\_(%D0%B1%D0%B5%D0%B7\_%D0%B7%D1%81%D1%83%D0%B2%D1%83))
- [https://www.geeksforgeeks.org/concatenation-of-two-strings-in-cpp/](https://www.geeksforgeeks.org/concatenation-of-two-strings-in-cpp/)
- [http://www.cplusplus.com/reference/cstring/strcat/](http://www.cplusplus.com/reference/cstring/strcat/)