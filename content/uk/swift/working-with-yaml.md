---
title:                "Робота з yaml"
html_title:           "Swift: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Що і для чого? 
Робота з YAML - це процес створення, читання та змінення даних у вигляді текстового формату. Програмісти використовують YAML для організації та збереження даних у структурованому форматі, доступному для машинного читання.

## Як працювати з YAML:
```Swift
import YAML

let myData = [
    "name": "John",
    "age": 30,
    "job": "Developer"
]

let yamlString = YAMLEncoder.encode(myData)
print(yamlString)

// Вивід:
// name: John
// age: 30
// job: Developer
```

## Глибоке дослідження:
Історичний контекст: YAML був розроблений у 2001 році як спрощений формат обміну даними, що змінив старіші формати як JSON та XML. Альтернативи: поширеним альтернативою для YAML є JSON, але YAML зазвичай має більш зрозумілу структуру та дозволяє коментувати дані. Деталі реалізації: YAML базується на простому структурному синтаксисі та застосовується у багатьох різних мов програмування.

## Див. також:
документація YAML {https://yaml.org/}