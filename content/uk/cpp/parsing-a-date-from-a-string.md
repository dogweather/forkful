---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Розбір дати з рядка - це процес перетворення формату дати, записаної як текст, на тип даних, який комп'ютер може розуміти та обробляти. Програмісти роблять це для завдань, що пов'язані з обробкою дат, таких як порівняння дат, додавання або віднімання днів, та інших.

## Як це зробити:

```C++
#include <iostream>
#include <iomanip> 
#include <sstream> 

int main() {
    std::string s = "24.10.2021";
    std::tm date = {};

    std::stringstream ss(s);

    ss >> std::get_time(&date, "%d.%m.%Y");

    std::cout << std::put_time(&date, "%Y-%m-%d") << "\n";
    return 0;
}
```
При виконанні цього коду виходом буде:
```C++
2021-10-24
```
## Пірнемо глибше:

Розбір дати з рядка є стандартною технікою в програмуванні з часів, коли ми почали використовувати комп'ютери для обробки дат і часу. Альтернативами можуть бути бібліотеки як Boost Date_Time, chrono і тд. В даних прикладах ми використовуємо стандартні бібліотеки C++. 

Ми використовуємо std::get_timeдля парсинга рядка до структури tm. Ця структура та std::put_time, яка використовується для форматування цієї структури у рядок, обидві є частиною <iomanip>, одного з стандартних заголовків C++, які використовуються в цій задачі.

## Дивіться також:

- [C++ reference на std::get_time](http://www.cplusplus.com/reference/iomanip/get_time/)
- [C++ reference на std::put_time](http://www.cplusplus.com/reference/iomanip/put_time/)
- [Boost Date_Time](https://www.boost.org/doc/libs/1_74_0/doc/html/date_time.html)
- [std::chrono](http://en.cppreference.com/w/cpp/chrono)

Примітка: Для розбору дати з рядка має різні можливості, і вибір правильного засобу залежить від специфічних вимог вашого проекту.