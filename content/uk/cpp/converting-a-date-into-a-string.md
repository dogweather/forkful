---
title:                "C++: Перетворення дати у рядок"
simple_title:         "Перетворення дати у рядок"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

Конвертація дати в рядок є важливою частиною багатьох програм, особливо тих, які працюють з даними про час. Це дає можливість користувачам зручно відображати дату у зрозумілому для них форматі.

## Як

Конвертація дати в рядок може бути виконана за допомогою функції `strftime` в стандартній бібліотеці мови C++. Ця функція дозволяє використовувати різні формати для відображення дати в рядку. Наприклад, для відображення поточної дати у форматі "день-місяць-рік" код буде виглядати так:

```C++
#include <iostream>
#include <ctime>
using namespace std;

int main() {
    time_t now = time(0);
    char str[80];
    struct tm * timeinfo = localtime(&now);
    strftime(str, 80, "%d-%m-%Y", timeinfo);
    cout << str << endl;
    return 0;
}
```

Результат виконання програми буде виглядати наступним чином: "03-07-2021".

## Глибоке дослідження

У C++ також є більш потужна бібліотека для роботи з датами та часом - `chrono`. Вона дозволяє працювати з різними типами даних, такими як `time_point` та `duration`. Для конвертації дати в рядок за допомогою цієї бібліотеки можна використовувати функцію `to_iso_string`, наприклад:

```C++
#include <iostream>
#include <chrono>
#include <string>
using namespace std;
using namespace chrono;

int main() {
    system_clock::time_point now = system_clock::now();
    string str = to_iso_string(now);
    cout << str << endl;
    return 0;
}
```

Результатом буде рядок у форматі "20210703T184332".

## Дивись також

- [Стандартна бібліотека C++](https://en.cppreference.com/w/cpp/header)
- [Бібліотека `chrono` у C++](https://en.cppreference.com/w/cpp/chrono)