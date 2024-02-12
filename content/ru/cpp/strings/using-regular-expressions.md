---
title:                "Использование регулярных выражений"
aliases: - /ru/cpp/using-regular-expressions.md
date:                  2024-01-29T00:03:54.417435-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Регулярные выражения – это шаблоны, используемые для сопоставления комбинаций символов в тексте. Программисты используют их для задач вроде валидации, поиска и манипуляции с текстом благодаря их мощности и гибкости.

## Как использовать:

Чтобы использовать регулярные выражения в C++, вам нужно подключить библиотеку `<regex>`. Вот как осуществить поиск, замену и сопоставление текста:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string target("Привет Мир. Это тест регулярного выражения.");
    
    // Сопоставление
    std::regex match_pattern("Привет Мир");
    bool is_match = std::regex_match(target, match_pattern);
    std::cout << (is_match ? "Совпадает" : "Не совпадает") << "\n";
    
    // Поиск
    std::regex search_pattern("\\bэто\\b");
    std::smatch matches;
    if (std::regex_search(target, matches, search_pattern)) {
        std::cout << "Найдено: " << matches[0] << "\n";
    }

    // Замена
    std::regex replace_pattern("Мир");
    std::string result = std::regex_replace(target, replace_pattern, "Вселенная");
    std::cout << "После замены: " << result << "\n";
    
    return 0;
}
```

Пример вывода:

```
Совпадает
Найдено: это
После замены: Привет Вселенная. Это тест регулярного выражения.
```

## Погружение

Регулярные выражения являются частью информатики с 1950-х годов, их популяризировали утилиты вроде grep в Unix. C++ принял их гораздо позже, с `std::regex` в C++11. Поддержка на уровне компиляторов варьируется; некоторые могут отставать в полной поддержке возможностей регулярных выражений.

Альтернативами `std::regex` являются библиотеки вроде Boost.Regex или PCRE (Perl Compatible Regular Expressions). Например, Boost.Regex часто превосходит по производительности `std::regex` и имеет более богатый набор возможностей.

С точки зрения реализации, `std::regex` может быть медленнее, чем некоторые пользовательские алгоритмы разбора, особенно для простых шаблонов. Понимание компромисса между удобством регулярных выражений и потенциальными проблемами производительности является ключевым.

## См. также

- Справка по C++ о `<regex>`: https://en.cppreference.com/w/cpp/regex
- Документация Boost.Regex: https://www.boost.org/doc/libs/release/libs/regex/
- Официальный сайт PCRE: https://www.pcre.org/

Дополнительные материалы и инструменты для улучшения ваших навыков в регулярных выражениях:

- Учебник Regular-Expressions.info: https://www.regular-expressions.info/tutorial.html
- Regex101 (онлайн-тестер): https://regex101.com/
