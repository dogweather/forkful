---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:08.174436-07:00
description: "\u041A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C: Arduino \u0438\u0437\u043D\u0430\u0447\u0430\u043B\u044C\u043D\
  \u043E \u043D\u0435 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u0438\u0432\u0430\
  \u0435\u0442 \u0440\u0430\u0431\u043E\u0442\u0443 \u0441 YAML. \u0414\u043B\u044F\
  \ \u0440\u0430\u0431\u043E\u0442\u044B \u0441 \u043D\u0438\u043C \u0438\u0441\u043F\
  \u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435 \u0432\u043D\u0435\u0448\u043D\u044E\
  \u044E \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443. \u041D\u0430\
  \u043F\u0440\u0438\u043C\u0435\u0440: \u0423\u0441\u0442\u0430\u043D\u043E\u0432\
  \u0438\u0442\u0435 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443\u2026"
lastmod: '2024-03-13T22:44:45.569704-06:00'
model: gpt-4-0125-preview
summary: "Arduino \u0438\u0437\u043D\u0430\u0447\u0430\u043B\u044C\u043D\u043E \u043D\
  \u0435 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u0438\u0432\u0430\u0435\u0442\
  \ \u0440\u0430\u0431\u043E\u0442\u0443 \u0441 YAML."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

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
