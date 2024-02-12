---
title:                "Преобразование даты в строку"
aliases:
- ru/cpp/converting-a-date-into-a-string.md
date:                  2024-01-28T23:56:14.569548-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование даты в строку"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Преобразование даты в строку в C++ позволяет превратить объект даты в формат удобочитаемого текста. Это ключевой момент для отображения дат пользователю и ведения журналов событий в удобной для восприятия форме.

## Как это сделать:
В современном C++ библиотеки `<chrono>` и `<iomanip>` являются вашими помощниками для операций с датой-временем. Вот быстрый метод с использованием `std::put_time`:

```cpp
#include <iostream>
#include <iomanip>
#include <chrono>
#include <sstream>

int main() {
    auto now = std::chrono::system_clock::now(); // Получить текущее время
    auto time = std::chrono::system_clock::to_time_t(now); // Преобразовать в time_t
    
    // Преобразовать в структуру tm для форматирования
    std::tm tm = *std::localtime(&time);

    // Строковый поток для вывода
    std::stringstream ss;

    ss << std::put_time(&tm, "%Y-%m-%d %H:%M:%S"); // Формат: ГГГГ-ММ-ДД ЧЧ:ММ:СС

    std::string date_str = ss.str(); // Преобразовать в строку

    std::cout << date_str << std::endl; // Вывести строку даты
    return 0;
}
```

Пример вывода (зависит от текущей даты и времени):
```
2023-03-15 14:25:30
```

## Подробнее
До появления `<chrono>`, программисты C++ часто сталкивались с необходимостью использовать управление временем в стиле C через `<ctime>`. Это было менее интуитивно понятно и более подвержено ошибкам из-за ручного управления памятью и зависимости от платформы.

Альтернативы `std::put_time` включают использование `strftime`, но это больше в стиле C. Сторонние библиотеки, такие как Boost.Date_Time, могут предложить больше функционала за счет добавления зависимостей.

Ключевой момент реализации — понимание спецификаторов формата в `std::put_time`, которые похожи на используемые в `strftime`. Вы отображаете заполнители на компоненты даты или времени — `%Y` для полного года, `%m` для месяца и так далее.

## Смотрите также
- [Документация `<chrono>`](https://en.cppreference.com/w/cpp/header/chrono)
- [Документация `<iomanip>`](https://en.cppreference.com/w/cpp/header/iomanip)
- [Boost.Date_Time](https://www.boost.org/doc/libs/release/libs/date_time/)
