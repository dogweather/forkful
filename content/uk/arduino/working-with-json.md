---
title:                "Робота з JSON"
date:                  2024-01-19
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"

category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Що це таке і чому?

JSON (JavaScript Object Notation) - простий формат обміну даними. Програмісти використовують його для зручного зберігання і передачі складних даних між сервісами та пристроями.

## Як це робити:

```Arduino
#include <ArduinoJson.h>  // Підключення бібліотеки для роботи з JSON

void setup() {
  Serial.begin(9600);

  // Створення об'єкта JSON
  DynamicJsonDocument doc(1024);
  doc["temp"] = 23.5;
  doc["humidity"] = 48.12;
  doc["status"] = "ok";
  
  // Серіалізація JSON та вивід у Серіальний порт
  serializeJson(doc, Serial);
}

void loop() {
  // тут нічого не потрібно робити
}
```
Вивід: ```{"temp":23.5,"humidity":48.12,"status":"ok"}```

## Поглиблене занурення:

JSON з'явився у 2001 році як спрощений підхід до XML. Альтернативи JSON включають XML, YAML і ProtoBuf. Для роботи з JSON на Arduino, використовується бібліотека ArduinoJson, яка дозволяє серіалізувати і десеріалізувати JSON-дані. Вибір версії бібліотеки ґрунтується на розмірі доступної пам'яті та необхідних функцій.

## Дивись також:

- Офіційний сайт ArduinoJson: https://arduinojson.org/
- ArduinoJson на GitHub: https://github.com/bblanchon/ArduinoJson
- Інтерактивний JSON серіалізатор/десеріалізатор: https://arduinojson.org/v6/assistant/
