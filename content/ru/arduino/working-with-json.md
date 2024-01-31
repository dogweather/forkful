---
title:                "Работа с JSON"
date:                  2024-01-29T00:04:52.824013-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Работа с JSON (JavaScript Object Notation) включает в себя манипуляцию данными, структурированными в лёгком для чтения и написания текстовом формате, который также удобен для анализа и генерации машинами. Программисты используют JSON в проектах для Arduino для общения с веб-сервисами, обмена данными и беспрепятственной настройки устройств.

## Как:

Для работы с JSON на Arduino вам понадобится библиотека ArduinoJson. Установите её через Менеджер Библиотек: Эскиз > Подключить Библиотеку > Управление Библиотеками..., затем найдите "ArduinoJson" и установите.

Вот простой пример разбора JSON:

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);

  DynamicJsonDocument doc(1024);
  deserializeJson(doc, json);

  const char* sensor = doc["sensor"];
  long time = doc["time"];
  double широта = doc["data"][0];
  double долгота = doc["data"][1];
  
  Serial.print("Датчик: ");
  Serial.println(sensor);
  Serial.print("Время: ");
  Serial.println(time);
  Serial.print("Широта: ");
  Serial.println(широта, 6);
  Serial.print("Долгота: ");
  Serial.println(долгота, 6);
}

void loop() {
  // В этом примере не используется.
}
```

Пример вывода:

```
Датчик: gps
Время: 1351824120
Широта: 48.756080
Долгота: 2.302038
```

Создание JSON:

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  DynamicJsonDocument doc(1024);

  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  doc["data"][0] = 48.756080;
  doc["data"][1] = 2.302038;

  serializeJson(doc, Serial);
}

void loop() {
  // В этом примере не используется.
}
```

Пример вывода:

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

## Подробнее

Библиотека ArduinoJson, авторства Benoit Blanchon, стала де-факто стандартом для манипуляции с JSON в Arduino. JSON приобрёл популярность за свою простоту по сравнению с XML, который был широко использован ранее. Существуют альтернативы, такие как MsgPack, но JSON остаётся в фаворитах за его текстовую читаемость и широкое применение. С точки зрения реализации, убедитесь, что вы выделили достаточно памяти для `DynamicJsonDocument`, чтобы избежать переполнения и используйте `StaticJsonDocument` для статических или объектов JSON известного размера.

## Смотри также

- Документация по библиотеке ArduinoJson: https://arduinojson.org/
- Официальный веб-сайт JSON: https://www.json.org/json-en.html
- Форум Arduino для обсуждений: https://forum.arduino.cc/
- Руководство по выбору между StaticJsonDocument и DynamicJsonDocument: https://arduinojson.org/documentation/memory-model/
