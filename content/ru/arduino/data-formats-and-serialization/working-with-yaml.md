---
title:                "Работа с YAML"
aliases:
- /ru/arduino/working-with-yaml/
date:                  2024-01-29T00:05:08.174436-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/arduino/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
YAML — это не язык разметки. Это удобный для людей стандарт сериализации данных для всех языков программирования. Программисты используют его для файлов конфигурации, обмена данными между языками, и он проще для понимания по сравнению с XML или JSON.

## Как использовать:
Arduino изначально не поддерживает работу с YAML. Для работы с ним используйте внешнюю библиотеку. Например:

Установите библиотеку "ArduinoJson" через Менеджер библиотек. Используйте `DynamicJsonDocument` для разбора:

```Arduino
#include <ArduinoJson.h>

const char* yaml = 
  "- title: Над пропастью во ржи\n"
  "  author: Дж.Д. Сэлинджер\n"
  "- title: 1984\n"
  "  author: Джордж Оруэлл\n";

void setup() {
  Serial.begin(9600);
  DynamicJsonDocument doc(1024);
  deserializeJson(doc, yaml);
  for (JsonObject elem : doc.as<JsonArray>()) {
    Serial.println(elem["title"].as<String>());
    Serial.println(elem["author"].as<String>());
  }
}

void loop() {
  // в этом примере не используется
}
```

Пример вывода:

```
Над пропастью во ржи
Дж.Д. Сэлинджер
1984
Джордж Оруэлл
```

## Погружение в детали
YAML появился в начале 2000-х годов, созданный для удобства чтения человеком. Будучи надмножеством JSON, любой файл JSON также является корректным YAML. Среди распространенных альтернатив - JSON или XML, но минималистичный синтаксис YAML направлен на лучшее управление для человека без лишней сложности. Работа с YAML на Arduino означает преобразование YAML в JSON с помощью внешних инструментов, а затем использование JSON в ваших скетчах.

## Смотрите также
- Официальный сайт YAML: https://yaml.org
- Репозиторий ArduinoJson на GitHub: https://github.com/bblanchon/ArduinoJson
- Онлайн конвертер YAML в JSON: https://www.json2yaml.com/
