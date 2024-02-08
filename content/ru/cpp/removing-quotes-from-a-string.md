---
title:                "Удаление кавычек из строки"
aliases:
- ru/cpp/removing-quotes-from-a-string.md
date:                  2024-01-29T00:01:48.394217-07:00
model:                 gpt-4-0125-preview
simple_title:         "Удаление кавычек из строки"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Удаление кавычек из строки означает избавление от тех надоедливых двойных или одинарных символов, которые окружают наш текст (' или "). Программисты часто делают это для очистки ввода, сохранения текста в базе данных или подготовки строк к дальнейшей обработке без лишнего мусора в виде кавычек.

## Как это сделать:
Вот простой способ избавиться от кавычек в C++:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Привет, 'Мир'!")";
    std::string no_quotes = remove_quotes(original);
    std::cout << no_quotes << std::endl;
    return 0;
}
```

Запустите это, и вы получите:

```
Привет, Мир!
```

Вуаля! Кавычки исчезли.

## Погружение в детали
Кавычки были неприятностью в текстах с самого зарождения вычислительной техники. Раньше видели, как программисты с трудом перебирали каждый символ, чтобы отфильтровать эти кавычки. Сегодня у нас есть `std::remove` в Стандартной Шаблонной Библиотеке (STL) для выполнения тяжелой работы.

Альтернативы? Конечно! Вы можете использовать регулярные выражения с `std::regex` для поиска кавычек, но это немного как использовать молоток для разбивания орешка - мощно, но может быть избыточно для простых задач. Те, кто предпочитает новые версии C++, могут попробовать `std::string_view` для подходов, не предполагающих изменений.

С точки зрения реализации, помните, что `std::remove` на самом деле не удаляет элементы из контейнера; он сдвигает неудаленные элементы вперед и возвращает итератор за новый конец диапазона. Вот почему нам нужен метод `erase` для отсечения нежелательного хвоста.

## Смотрите также
- Справочник по C++ `std::remove`: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- Больше о манипуляциях со строками в `std::string`: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
