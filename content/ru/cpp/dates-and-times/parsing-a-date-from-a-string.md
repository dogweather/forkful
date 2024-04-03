---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:08.108565-07:00
description: "\u041A\u0430\u043A: \u0418\u0441\u043F\u043E\u043B\u044C\u0437\u0443\
  \u0439\u0442\u0435 `<chrono>` \u0438 `<sstream>`, \u0447\u0442\u043E\u0431\u044B\
  \ \u0440\u0430\u0437\u043E\u0431\u0440\u0430\u0442\u044C \u0434\u0430\u0442\u0443\
  \ \u0432 C++. \u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u044B\u0439 \u043F\
  \u0440\u0438\u043C\u0435\u0440."
lastmod: '2024-03-13T22:44:45.624861-06:00'
model: gpt-4-0125-preview
summary: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435 `<chrono>`\
  \ \u0438 `<sstream>`, \u0447\u0442\u043E\u0431\u044B \u0440\u0430\u0437\u043E\u0431\
  \u0440\u0430\u0442\u044C \u0434\u0430\u0442\u0443 \u0432 C++."
title: "\u0410\u043D\u0430\u043B\u0438\u0437 \u0434\u0430\u0442\u044B \u0438\u0437\
  \ \u0441\u0442\u0440\u043E\u043A\u0438"
weight: 30
---

## Как:
Используйте `<chrono>` и `<sstream>`, чтобы разобрать дату в C++. Вот быстрый пример:

```C++
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_text = "2023-04-01";
    std::istringstream ss(date_text);
    std::chrono::year_month_day parsed_date;
    
    ss >> std::chrono::parse("%F", parsed_date);
    if (ss.fail()) {
        std::cout << "Ошибка разбора\n";
        return 1;
    }

    std::cout << "Год: " << int(parsed_date.year()) << '\n';
    std::cout << "Месяц: " << unsigned(parsed_date.month()) << '\n';
    std::cout << "День: " << unsigned(parsed_date.day()) << '\n';

    return 0;
}
```

Пример вывода:
```
Год: 2023
Месяц: 4
День: 1
```

## Подробнее
Разбор дат из строк не новость. Еще в дни C был типичен `strptime`. В современном C++, `<chrono>` - ваш друг. Он аккуратно разделяет заботы: форматирование/разбор с `std::chrono::parse`, и манипуляции с датами с помощью типов `std::chrono`.

До C++20 вы, скорее всего, использовали бы `std::get_time` или сторонние библиотеки, например, Boost. После C++20 стандартная библиотека получила обновление с улучшениями `std::chrono`. Теперь у вас есть типобезопасные типы дат и функции "из коробки".

Функция разбора, `std::chrono::parse`, универсальна, понимает много форматов даты и времени. Формат "%F", который мы использовали выше, - это формат даты ISO 8601 (год-месяц-день). Но вы также можете обрабатывать и другие, просто настроив строку формата соответственно.

Помните, несмотря на надежный разбор, пользовательский ввод сложен. Всегда корректно обрабатывайте ошибки разбора, как это сделано с `ss.fail()` в примере.

## См. также
Углубитесь в `<chrono>` с официальной [страницей cppreference](https://en.cppreference.com/w/cpp/header/chrono).

Получите исторический контекст из мнения Страуструпа о истории C++ в [Design and Evolution of C++](http://www.stroustrup.com/hopl2.pdf).

Для крайних случаев или нестандартных форматов рассмотрите [Boost.DateTime](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html).
