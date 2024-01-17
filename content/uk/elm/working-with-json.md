---
title:                "Робота з json"
html_title:           "Elm: Робота з json"
simple_title:         "Робота з json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Що & Чому?

Робота з JSON - це процес обробки та збереження даних у форматі, який зручний для обміну між різними програмами. Програмісти часто використовують JSON оскільки це простий та широко підтримуваний формат.

## Як:

```Elm
import Json.Decode as Decode

-- Декодування JSON у структуру з типом "мова":
Decode.decodeString Decode.string """ "Ukrainian" """
--> Ok "Ukrainian"

-- Кодування структури з типом "мова" у формат JSON:
Decode.encode 1.2 "Ukrainian"
--> "\"Ukrainian\""
```

## Розглянути детальніше:

- **Історичний контекст**: JSON був створений у 2001 році та швидко став популярним у світі розробки програмного забезпечення.
- **Альтернативи**: Інші формати даних, такі як XML та CSV, також використовуються для збереження та обміну даними.
- **Деталі реалізації**: Elm має вбудовану підтримку для роботи з JSON через модуль ```Json.Decode```.

## Дивись також:
- [Офіційна документація Elm для роботи з JSON](https://guide.elm-lang.org/json/)
- [Стаття про переваги використання JSON у програмуванні](https://www.itpro.co.uk/network-internet/30494/what-is-json-everything-you-need-to-know)