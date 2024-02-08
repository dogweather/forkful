---
title:                "Робота з JSON"
aliases:
- uk/arduino/working-with-json.md
date:                  2024-02-03T19:21:55.950401-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/arduino/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?

JSON, або JavaScript Object Notation, це легковагий формат обміну даними, що робить його ідеальним для зберігання даних або файлів конфігурації в проектах Arduino. Програмісти використовують його через його простоту та читабельність в різних програмувальних середовищах, включаючи Arduino, що дозволяє забезпечувати безшовний обмін даними з веб API або іншими системами.

## Як:

Щоб працювати з JSON в Arduino, бібліотека `ArduinoJson` є популярним вибором через її простоту використання та ефективність. Вона дозволяє парсити JSON-рядки, модифікувати їх і серіалізувати об'єкти назад у JSON-рядки. Ось як її використовувати:

1. **Встановіть бібліотеку ArduinoJson**: Використовуйте менеджер бібліотек в Arduino IDE та встановіть "ArduinoJson".

2. **Десеріалізація JSON-рядка**: Ось як парсити JSON-рядок та витягувати значення.

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);
  StaticJsonDocument<200> doc; // Регулюйте розмір відповідно до JSON документа
  DeserializationError error = deserializeJson(doc, json);

  if (error) {
    Serial.print(F("deserializeJson() failed: "));
    Serial.println(error.f_str());
    return;
  }

  const char* sensor = doc["sensor"]; // "gps"
  long time = doc["time"]; // 1351824120
  float широта = doc["data"][0]; // 48.756080
  float довгота = doc["data"][1]; // 2.302038
  
  Serial.println(sensor);
  Serial.println(time);
  Serial.println(latitude, 6);
  Serial.println(longitude, 6);
}

void loop() {
  // Порожній цикл
}
```

Приклад виводу:

```
gps
1351824120
48.756080
2.302038
```

3. **Серіалізація в JSON-рядок**: Ось як створити JSON-рядок з даних.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc; // Регулюйте розмір відповідно до даних
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  JsonArray data = doc.createNestedArray("data");
  data.add(48.756080);
  data.add(2.302038);

  serializeJson(doc, Serial);
}

void loop() {
  // Порожній цикл
}
```

Приклад виводу (відформатовано для кращої читабельності):

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

Ефективне використання бібліотеки `ArduinoJson` дозволяє проектам Arduino спілкуватися з складними структурами даних в людсько-зрозумілому форматі, сприяючи розробці та інтеграції з веб-службами.
