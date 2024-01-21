---
title:                "Перетворення дати в рядок"
date:                  2024-01-20T17:36:08.910253-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (## Що та Чому?)
Перетворення дати в рядок – це процес форматування об'єкта дати в зрозумілий та читабельний формат. Програмісти роблять це для виведення дат у зручному для користувача форматі або для їх запису у файли та бази даних.

## How to: (## Як це зробити:)
```C++
#include <iostream>
#include <iomanip>
#include <chrono>

int main() {
    // Отримуємо поточну дату-час
    auto now = std::chrono::system_clock::now();
    std::time_t t = std::chrono::system_clock::to_time_t(now);

    // Конвертовано у tm як локальний час
    std::tm* localtm = std::localtime(&t);
    
    // Форматуємо та конвертуємо дату у рядок
    std::cout << std::put_time(localtm, "%Y-%m-%d %H:%M:%S") << std::endl;
}

// Вивід, наприклад: 2023-03-14 21:46:10
```

## Deep Dive (## Під водою)
У мові C++ робота з датою і часом має давню історію. Раніше використовувалися структури та функції з бібліотеки `<ctime>`. З появою C++11 з'явилася бібліотека `<chrono>`, що надає більш потужні та безпечні засоби для роботи з часом.

Функція `std::put_time` використовується для форматування часу. Це сучасний аналог функції `strftime`, але безпечніший і простіший у використанні.

Альтернативою цьому може бути власна реалізація конвертера, але вона зайве ускладнить код. Тим більше, коли стандартна бібліотека мови C++ надає необхідні інструменти.

## See Also (## Дивіться також)
- [cppreference.com: std::chrono](https://en.cppreference.com/w/cpp/chrono)
- [cppreference.com: std::put_time](https://en.cppreference.com/w/cpp/io/manip/put_time)
- [C++ Super-FAQ: Dates and Times](https://isocpp.org/wiki/faq/datetime)