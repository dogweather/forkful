---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:26.665114-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 C++ \u043D\u0435\u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438\
  \ \u0434\u043B\u044F JSON, \u0430\u043B\u0435 \u0441\u0442\u043E\u0440\u043E\u043D\
  \u043D\u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u044F\
  \u043A-\u043E\u0442 nlohmann/json, \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\
  \u0435 \u043F\u0440\u043E\u0441\u0442\u0438\u043C. \u041E\u0441\u044C \u044F\u043A\
  \ \u043C\u043E\u0436\u043D\u0430 \u0457\u0457\u2026"
lastmod: '2024-03-13T22:44:49.883897-06:00'
model: gpt-4-0125-preview
summary: "\u0423 C++ \u043D\u0435\u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\
  \u0432\u0430\u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438\
  \ \u0434\u043B\u044F JSON, \u0430\u043B\u0435 \u0441\u0442\u043E\u0440\u043E\u043D\
  \u043D\u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438, \u044F\
  \u043A-\u043E\u0442 nlohmann/json, \u0440\u043E\u0431\u043B\u044F\u0442\u044C \u0446\
  \u0435 \u043F\u0440\u043E\u0441\u0442\u0438\u043C."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

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
