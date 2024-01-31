---
title:                "Робота з JSON"
date:                  2024-01-19
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?
JSON (JavaScript Object Notation) - це текстовий формат обміну даними. Програмісти використовують його для зручної передачі структурованих даних між різними мовами програмування та сервісами.

## Як це робити:
В C++ для роботи з JSON часто використовують бібліотеку nlohmann/json. Ось як можна її використати:

```C++
#include "json.hpp" // Підключаємо бібліотеку nlohmann/json
#include <iostream>

int main() {
    // Створення та серіалізація JSON об'єкта
    nlohmann::json j;
    j["name"] = "Ivan";
    j["age"] = 30;
    j["is_programmer"] = true;
    
    // Виведення JSON як string
    std::cout << j.dump(4) << std::endl;
    
    // Десеріалізація JSON зі string
    auto parsed = nlohmann::json::parse(R"({"city":"Kyiv","population":2884000})");
    std::cout << "City: " << parsed["city"] << std::endl;
    std::cout << "Population: " << parsed["population"] << std::endl;
    
    // Використання JSON як мапи
    for (auto& element : j.items()) {
        std::cout << element.key() << " : " << element.value() << std::endl;
    }
    
    return 0;
}
```

Приклад виводу:

```
{
    "age": 30,
    "is_programmer": true,
    "name": "Ivan"
}
City: Kyiv
Population: 2884000
age : 30
is_programmer : true
name : Ivan
```

## Поглиблені знання:
JSON з'явився у 2001 році як альтернатива XML. Його переваги - простота та легка читабельність. Існують альтернативи, як-от YAML чи BSON, але JSON залишається популярним через свою універсальність та підтримку у багатьох мовах програмування. На C++ робота з JSON часто реалізується через зовнішні бібліотеки, такі як nlohmann/json, jsoncpp, або RapidJSON, які забезпечують парсинг і серіалізацію даних.

## Також дивіться:
- Документація бібліотеки nlohmann/json: https://github.com/nlohmann/json
- JSON стандарт: https://www.json.org/json-uk.html
- Співставлення JSON бібліотек для C++: https://en.cppreference.com/w/cpp/links/libs
