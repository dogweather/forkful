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

# Чому

JSON є популярним форматом для зберігання та передавання даних між різними програмами та пристроями. Завдяки його простоті та масштабуванню, він знаходить широке застосування у IoT-проектах, зокрема з мікроконтролерів, таких як Arduino.

# Як

Для початку роботи з JSON в Arduino, потрібно встановити бібліотеку "ArduinoJson". Вона доступна в бібліотеках Arduino IDE або може бути завантажена з GitHub.

```Arduino
#include <ArduinoJson.h> // підключаємо бібліотеку

void setup() {
   // ініціалізуємо серійний порт для виводу даних
   Serial.begin(9600);
   
   // створюємо об'єкт JsonDocument з розміром 200 байт
   StaticJsonDocument<200> jsonDoc;
   
   // додаємо значення до об'єкта
   jsonDoc["name"] = "John";
   jsonDoc["age"] = 25;
   jsonDoc["status"] = "active";
   
   // перетворюємо об'єкт в рядок JSON
   String jsonData;
   serializeJson(jsonDoc, jsonData);
   
   // виводимо рядок JSON в серійний порт
   Serial.println(jsonData);
}

void loop() {
   // код для зчитування даних та роботи з JSON
}
```

Вище наведений приклад додає три пари "ключ-значення" до об'єкта `jsonDoc` та перетворює його в рядок JSON за допомогою функції `serializeJson`. Результат можна побачити в серійному порті, що дозволяє зручно відстежувати та перевіряти дані.

# Deep Dive

Крім стандартних операцій додавання та зчитування значень до об'єкта, бібліотека "ArduinoJson" також підтримує різні методи для роботи з JSON. Деякі з них:

- `getMember()`: повертає значення за вказаним ключем у вигляді `JsonVariant`, який може бути автоматично перетворений у необхідний тип даних.
- `containsKey()`: перевіряє наявність певного ключа в об'єкті.
- `remove()`: видаляє певну пару "ключ-значення" з об'єкта.

Детальну документацію з повним списком функцій та прикладів можна знайти на офіційному сайті бібліотеки ArduinoJson.

# Дивіться також

- [Офіційний сайт бібліотеки ArduinoJson](https://arduinojson.org)
- [Приклади роботи з JSON в Arduino](https://github.com/bblanchon/ArduinoJson/tree/6.x/examples)