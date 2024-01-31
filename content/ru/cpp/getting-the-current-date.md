---
title:                "Получение текущей даты"
date:                  2024-01-28T23:58:17.876832-07:00
model:                 gpt-4-0125-preview
simple_title:         "Получение текущей даты"

category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Получение текущей даты в вашей программе C++ может быть полезным: подумайте о ведении журнала, временных метках или функциях планирования. Речь идет о соответствии настоящему - ваше программное обеспечение знает сегодняшний день так же хорошо, как и вы.

## Как:
Вот как получить текущую дату с помощью `<chrono>` — современно, чисто, без лишних слов.

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    // Получаем текущее системное время
    auto now = std::chrono::system_clock::now();

    // Преобразуем в time_t, затем в tm для удобочитаемого формата
    std::time_t now_c = std::chrono::system_clock::to_time_t(now);
    std::tm* now_tm = std::localtime(&now_c);

    // Выводим в формате ГГГГ-ММ-ДД
    std::cout << (now_tm->tm_year + 1900) << '-' 
              << (now_tm->tm_mon + 1) << '-'
              <<  now_tm->tm_mday << '\n';

    return 0;
}
```

Пример вывода, который вы получите сегодня:
```
2023-4-14
```
Ничего особенного, выполняет свою работу.

## Подробнее
В старые времена господствовали функции времени C — `<ctime>` был вашим выбором. Но с C++11 и позже, `<chrono>` вышел на передний план. Он типобезопасен и избегает общих ошибок с устаревшими функциями C.

Альтернативы? Конечно. Вы могли бы использовать старый `std::time` или даже специфичные для ОС API, если вам нравится жить на грани (или у вас есть очень конкретные потребности).

А что насчет деталей реализации? `<chrono>` представляет моменты времени, продолжительности и часы. Он точен и тщательно спроектирован. Время сложно (високосные секунды, часовые пояса), и `<chrono>` обрабатывает эту сложность под капотом, так что вам не приходится с этим мучиться.

## Смотрите также
- [C++ Справочник - библиотека `<chrono>`](https://en.cppreference.com/w/cpp/chrono)
- [C++ Справочник - Устаревший `<ctime>`](https://en.cppreference.com/w/cpp/header/ctime)
- Для глубокого погружения ознакомьтесь с библиотекой дат Howard Hinnant, расширением `<chrono>`: [https://github.com/HowardHinnant/date](https://github.com/HowardHinnant/date)
- Если вам когда-либо понадобится поддержка часовых поясов "прямо из коробки", попробуйте это: [https://en.cppreference.com/w/cpp/chrono/current_zone](https://en.cppreference.com/w/cpp/chrono/current_zone)
