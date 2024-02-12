---
title:                "Робота з JSON"
date:                  2024-02-03T19:22:26.665114-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

JSON (JavaScript Object Notation) — це легковаговий формат для зберігання та передачі даних, що робить його відмінним засобом для обміну даними між серверами та веб-додатками. Програмісти використовують JSON через його легкість для читання людиною та простоту парсингу машиною, особливо під час роботи над додатками, що потребують обміну даними через інтернет або налаштування конфігурацій.

## Як це зробити:

У C++ немає вбудованої підтримки для JSON, але сторонні бібліотеки, як-от nlohmann/json, роблять це простим. Ось як можна її використовувати для базових завдань:

Перш за все, переконайтеся, що у вас встановлена бібліотека. Якщо ви використовуєте менеджер пакунків, такий як vcpkg або Conan, ви легко можете додати `nlohmann/json` до свого проєкту.

### Парсинг JSON з рядка

```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // JSON дані як рядок
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // Парсинг рядка JSON
    auto jsonObject = nlohmann::json::parse(jsonData);

    // Доступ до даних
    std::cout << "Ім'я: " << jsonObject["name"] << "\n"
              << "Вік: " << jsonObject["age"] << "\n"
              << "Місто: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**Приклад виводу:**

```
Ім'я: John
Вік: 30
Місто: New York
```

### Генерація JSON

Створення даних JSON є також простим; вам лише потрібно присвоїти значення об'єкту `nlohmann::json`.

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // Створення об'єкта JSON
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // Конвертація об'єкта JSON у рядок та його друк
    std::string jsonString = jsonObject.dump(4); // Аргумент 4 для красивого виводу
    std::cout << jsonString << std::endl;

    return 0;
}
```

**Приклад виводу:**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

Ці приклади демонструють основну функціональність роботи з JSON в C++ за допомогою бібліотеки `nlohmann/json`. З цими основами ви можете парсити та генерувати JSON для різних додатків, від файлів конфігурації до обміну даними в мережевих додатках.
