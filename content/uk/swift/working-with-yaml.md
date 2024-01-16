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

## Чому

Якщо ви працюєте з багатовимірними даними або управляєте конфігураційними файлами, то ви, напевно, чули про формат YAML. Це простий і структурований формат, який стає все більш популярним серед розробників. Він дозволяє зберігати дані у зручному для людини вигляді, що робить його ідеальним для здійснення обміну даними між різними програмами та платформами.

## Як працювати з YAML в Swift

Як великий фанат Swift, не дивно, що ви хочете використовувати його для роботи з YAML. Для цього вам знадобиться зовнішня бібліотека, наприклад, `Yams`, яка дозволяє здійснювати парсинг та генерацію YAML даних. Ось приклад того, як використовувати бібліотеку `Yams` для створення та запису даних у форматі YAML:

```Swift
import Yams

// Створення об'єкта типу [String: Any], який буде перетворений у формат YAML
let data: [String: Any] = [
    "name": "John Smith",
    "age": 30,
    "languages": ["Swift", "Java", "Python"]
]

// Перетворення даних у формат YAML
let yaml = try Yams.dump(object: data)

// Запис даних у файл
try yaml.write(toFile: "personal_info.yaml", atomically: true, encoding: .utf8)
```

Тепер після запуску цих декількох рядків коду у вашій папці з'явиться файл `personal_info.yaml` з таким змістом:

```yaml
name: John Smith
age: 30
languages:
    - Swift
    - Java
    - Python
```

Зверніть увагу, що бібліотека `Yams` автоматично форматує дані у відповідному YAML стильові, що робить їх легко читабельними.

## Глибше в YAML

Тепер, коли ви вже знаєте, як працювати з YAML у Swift, можливо, ви ставите питання, які ще можливості у цьому форматі? Окрім збереження простих даних, YAML дозволяє вам створювати складні структури, такі як масиви та об'єкти, вкладаючи їх один у одного. Крім того, ви можете використовувати коментарі, які починаються з символу `#`, для пояснення даних або вказівки для їхнього використання.

Як любий інший формат, YAML має свої особливості та правила, які необхідно дотримуватися. Наприклад, всі вкладені об'єкти потрібно відділяти від зовнішнього об'єк