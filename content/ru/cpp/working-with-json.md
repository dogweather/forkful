---
title:                "Работа с JSON"
aliases:
- ru/cpp/working-with-json.md
date:                  2024-01-29T00:04:13.775770-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/cpp/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
