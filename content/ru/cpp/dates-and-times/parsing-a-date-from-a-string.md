---
title:                "Анализ даты из строки"
aliases: - /ru/cpp/parsing-a-date-from-a-string.md
date:                  2024-01-29T00:00:08.108565-07:00
model:                 gpt-4-0125-preview
simple_title:         "Анализ даты из строки"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Разбор даты из строки означает преобразование текста в тип данных даты. Программисты делают это для обработки логики, связанной с датами, в стандартизированной, независимой от локали форме, часто для задач, таких как проверка ввода, сортировка и хранение.

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
