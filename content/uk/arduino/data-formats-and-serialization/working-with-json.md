---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:55.950401-07:00
description: "JSON, \u0430\u0431\u043E JavaScript Object Notation, \u0446\u0435 \u043B\
  \u0435\u0433\u043A\u043E\u0432\u0430\u0433\u0438\u0439 \u0444\u043E\u0440\u043C\u0430\
  \u0442 \u043E\u0431\u043C\u0456\u043D\u0443 \u0434\u0430\u043D\u0438\u043C\u0438\
  , \u0449\u043E \u0440\u043E\u0431\u0438\u0442\u044C \u0439\u043E\u0433\u043E \u0456\
  \u0434\u0435\u0430\u043B\u044C\u043D\u0438\u043C \u0434\u043B\u044F \u0437\u0431\
  \u0435\u0440\u0456\u0433\u0430\u043D\u043D\u044F \u0434\u0430\u043D\u0438\u0445\
  \ \u0430\u0431\u043E \u0444\u0430\u0439\u043B\u0456\u0432 \u043A\u043E\u043D\u0444\
  \u0456\u0433\u0443\u0440\u0430\u0446\u0456\u0457 \u0432 \u043F\u0440\u043E\u0435\
  \u043A\u0442\u0430\u0445\u2026"
lastmod: '2024-03-13T22:44:49.805836-06:00'
model: gpt-4-0125-preview
summary: "JSON, \u0430\u0431\u043E JavaScript Object Notation, \u0446\u0435 \u043B\
  \u0435\u0433\u043A\u043E\u0432\u0430\u0433\u0438\u0439 \u0444\u043E\u0440\u043C\u0430\
  \u0442 \u043E\u0431\u043C\u0456\u043D\u0443 \u0434\u0430\u043D\u0438\u043C\u0438\
  , \u0449\u043E \u0440\u043E\u0431\u0438\u0442\u044C \u0439\u043E\u0433\u043E \u0456\
  \u0434\u0435\u0430\u043B\u044C\u043D\u0438\u043C \u0434\u043B\u044F \u0437\u0431\
  \u0435\u0440\u0456\u0433\u0430\u043D\u043D\u044F \u0434\u0430\u043D\u0438\u0445\
  \ \u0430\u0431\u043E \u0444\u0430\u0439\u043B\u0456\u0432 \u043A\u043E\u043D\u0444\
  \u0456\u0433\u0443\u0440\u0430\u0446\u0456\u0457 \u0432 \u043F\u0440\u043E\u0435\
  \u043A\u0442\u0430\u0445 Arduino."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

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
