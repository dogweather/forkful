---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що це і навіщо?

Порівняння двох дат - це процес визначення, яка з двох дат є ранішою або пізнішою. Програмісти роблять це, коли їм потрібно сортувати події за датою або визначити проміжок часу між двома датами.

## Як це зробити:

```C++
#include<iostream>
#include<ctime>
using namespace std;

int main(){
    // Отримання поточного часу
    time_t now = time(0);
    tm *dt = localtime(&now);

    // Отримання дати у форматі року, місяця та дня
    int year = 1900 + dt->tm_year;
    int month = 1 + dt->tm_mon;
    int day = dt->tm_mday;

    cout << "Сьогодні: " << day << "-" << month << "-" << year << endl;

    // Припустимо, що ми маємо іншу дату 
    int other_day = 25;
    int other_month = 12;
    int other_year = 2030;

    // Порівнюємо дві дати
    if(other_year > year || 
       (other_year == year && other_month > month) || 
       (other_year == year && other_month == month && other_day > day))
        {
            cout << "Вказана дата є пізнішою." << endl;
        }
    else
        {
            cout << "Вказана дата не є пізнішою." << endl;
        }
    return 0;
}
```

Цей код виведе результат у форматі:

```
Сьогодні: 3-4-2025
Вказана дата є пізнішою.
```

## Глибше занурення

Порівняння дат пройшло довгий шлях від простого порівняння днів, місяців і років. Сучасні мови програмування, такі як C++, надають можливість використовувати функції бібліотеки `<ctime>` для автоматичного отримання поточного часу та дати. Альтернатива - використовувати зовнішні бібліотеки, але велосипеди тут не потрібні.

Зверніть увагу, що функція `localtime()` вказує на структуру, яка ініціюється статично і може бути перезаписана кожного разу при виклику функції.

## Дивіться також

- [`<ctime>` бібліотека](https://www.cplusplus.com/reference/ctime/)
- [Функція `localtime()`](https://www.cplusplus.com/reference/ctime/localtime/)