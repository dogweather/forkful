---
title:                "Розбір дати з рядка"
aliases:
- uk/cpp/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:59.348315-07:00
model:                 gpt-4-0125-preview
simple_title:         "Розбір дати з рядка"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?
Розбір дати з рядка передбачає інтерпретацію формату рядка для вилучення компонентів дати, таких як день, місяць та рік. Програмісти роблять це, щоб обробляти введення користувача, читати файли даних або взаємодіяти з API, які передають дати у форматі рядків. Це важливо для обробки даних, валідації та виконання арифметичних операцій з датами в додатках.

## Як:
У сучасному C++ ви можете використовувати бібліотеку `<chrono>` для роботи з датами та часом нативно, але вона безпосередньо не підтримує розбір з рядків без ручного аналізу для більш складних форматів. Однак, для форматів дат ISO 8601 та простих налаштовуваних форматів, ось як ви можете досягнути розбору.

**Використовуючи `<chrono>` та `<sstream>`:**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // Формат ISO 8601
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day parsed_date;
    iss >> std::chrono::parse("%F", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Розібрана дата: " << parsed_date << std::endl;
    } else {
        std::cout << "Не вдалося розібрати дату." << std::endl;
    }
    
    return 0;
}
```
Приклад виводу:
```
Розібрана дата: 2023-04-15
```

Для більш складних форматів або коли працюєте зі старими версіями C++, популярними є сторонні бібліотеки, такі як `date.h` (бібліотека дати Говарда Гіннанта). Ось як ви можете розібрати різні формати з її допомогою:

**Використовуючи бібліотеку `date.h`:**
Переконайтеся, що у вас встановлена бібліотека. Знайти її можна [тут](https://github.com/HowardHinnant/date).

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "April 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days parsed_date;
    iss >> date::parse("%B %d, %Y", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Розібрана дата: " << parsed_date << std::endl;
    } else {
        std::cout << "Не вдалося розібрати дату з рядка." << std::endl;
    }

    return 0;
}
```
Приклад виводу (може варіюватися в залежності від локалі вашої системи та налаштувань дати):
```
Розібрана дата: 2023-04-15
```
