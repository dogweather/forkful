---
title:                "Робота з json"
html_title:           "Arduino: Робота з json"
simple_title:         "Робота з json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Що це таке та чому це потрібно?

Робота з JSON - це метод передачі інформації між різними програмами та пристроями. Програмісти користуються цим для обміну даними, такими як малюнки, текст або структурована інформація.

## Як це зробити:

### Створення об'єкту JSON
```Arduino

// Приклад створення об'єкту JSON
#include <ArduinoJson.h>

// Створення об'єкту JSON
// Ключеве слово "char" вказує на тип даних - рядок
char json[] = "{\"name\":\"John\", \"age\":30}";

// Додавання значення до об'єкту
StaticJsonDocument<200> doc;
doc["surname"] = "Smith";
doc["occupation"] = "Programmer";
serializeJson(doc, json);
```

### Парсинг JSON
```Arduino

// Приклад парсингу JSON
#include <ArduinoJson.h>

// Використання об'єкту JSON для отримання значень
StaticJsonDocument<200> doc;
deserializeJson(doc, json);

// Отримання значень з об'єкту
const char* name = doc["name"];
int age = doc["age"];
```

## Глибинне вивчення:

### Історичний контекст
JSON був створений у 2001 році Дугласом Крокфордом та широко використовується веб-розробниками для передачі даних між сервером та клієнтом.

### Альтернативи
Хоча JSON є популярним методом, існують й інші формати для передачі даних, такі як XML або YAML.

### Деталі реалізації
У бібліотеці ArduinoJson є декілька методів для роботи з JSON, включаючи створення та парсинг об'єктів, додавання і отримання значень, а також перевірка на наявність та валідацію об'єкту.

## Додаткові джерела:
- [Офіційна документація ArduinoJson](https://arduinojson.org)
- [Вступ до роботи з JSON на Arduino](https://create.arduino.cc/projecthub/SuryaPuranik/introduction-to-json-using-arduino-955fa1)