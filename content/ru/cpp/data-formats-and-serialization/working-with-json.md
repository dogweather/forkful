---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:13.775770-07:00
description: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON (JavaScript Object\
  \ Notation) \u043D\u0430 C++ \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0432\
  \ \u0441\u0435\u0431\u044F \u0440\u0430\u0437\u0431\u043E\u0440 \u0438 \u0433\u0435\
  \u043D\u0435\u0440\u0430\u0446\u0438\u044E \u0442\u0435\u043A\u0441\u0442\u043E\u0432\
  \u044B\u0445 \u0434\u0430\u043D\u043D\u044B\u0445 \u0432 \u0444\u043E\u0440\u043C\
  \u0430\u0442\u0435 JSON. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\
  \u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442 JSON \u0434\
  \u043B\u044F\u2026"
lastmod: '2024-03-11T00:14:19.283521-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON (JavaScript Object Notation)\
  \ \u043D\u0430 C++ \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0432 \u0441\
  \u0435\u0431\u044F \u0440\u0430\u0437\u0431\u043E\u0440 \u0438 \u0433\u0435\u043D\
  \u0435\u0440\u0430\u0446\u0438\u044E \u0442\u0435\u043A\u0441\u0442\u043E\u0432\u044B\
  \u0445 \u0434\u0430\u043D\u043D\u044B\u0445 \u0432 \u0444\u043E\u0440\u043C\u0430\
  \u0442\u0435 JSON. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\
  \u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442 JSON \u0434\u043B\
  \u044F\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
---

{{< edit_this_page >}}

## Что и Почему?

Работа с JSON (JavaScript Object Notation) на C++ включает в себя разбор и генерацию текстовых данных в формате JSON. Программисты используют JSON для упрощённого обмена данными между серверами и веб-клиентами, поскольку он удобочитаем и независим от языка программирования.

## Как это сделать:

Для работы с JSON на C++ вам понадобится использовать библиотеку, например, `nlohmann/json`. Вот как можно разобрать и сгенерировать данные JSON:

```C++
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // Разбор JSON
    std::string str = R"({"name":"John", "age":30, "city":"New York"})";
    nlohmann::json parsed = nlohmann::json::parse(str);

    // Доступ к элементам
    std::cout << "Имя: " << parsed["name"] << std::endl;
    std::cout << "Возраст: " << parsed["age"] << std::endl;

    // Генерация JSON
    nlohmann::json j;
    j["name"] = "Jane";
    j["age"] = 25;
    j["city"] = "Los Angeles";

    std::cout << "Сгенерированный JSON: " << j.dump(4) << std::endl;

    return 0;
}
```

Пример вывода:
```
Имя: John
Возраст: 30
Сгенерированный JSON: {
    "age": 25,
    "city": "Los Angeles",
    "name": "Jane"
}
```

## Подробнее:

JSON был представлен как простой текстовый формат для обмена данными и стал стандартом благодаря своей простоте и широкому распространению. Существуют альтернативы, например, XML, но JSON лидирует в веб-API благодаря меньшему количеству вербализма и лучшей читаемости. C++ не имеет встроенной поддержки JSON, поэтому библиотеки вроде `nlohmann/json` популярны для обработки сериализации и десериализации, предлагая чистый API, который имитирует работу с нативными типами данных.

## Смотрите также:

- GitHub-репозиторий для `nlohmann/json`: https://github.com/nlohmann/json
- Официальный сайт JSON для дополнительной информации о формате: https://www.json.org/json-en.html
- Для работы с XML на C++: https://pugixml.org/
- Страница на Cppreference о потоках строк для продвинутой работы со строками на C++: https://en.cppreference.com/w/cpp/io/basic_stringstream
