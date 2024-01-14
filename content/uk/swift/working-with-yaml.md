---
title:                "Swift: Робота з yaml"
simple_title:         "Робота з yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

## Чому

Багато програмістів доступно зіткнуться з форматом даних YAML у своїй роботі. YAML (YAML Ain't Markup Language) є особливо корисним для зберігання конфігураційних параметрів і використання його в сучасних розробках.

## Як це зробити

Найпоширенішим способом роботи з YAML в Swift є використання фреймворку Yams. Для початку необхідно встановити фреймворк, використовуючи менеджер залежностей Swift, такий як Cocoapods або Swift Package Manager.

```Swift
import Yams

// Парсинг YAML з рядка
let string = """
- name: John
  age: 30
  occupation: Developer
"""

let data = try Yams.load(yaml: string)
print(data) // ["name": "John", "age": 30, "occupation": "Developer"]

// Створення YAML з об'єкта
struct Person: Codable {
    let name: String
    let age: Int
    let occupation: String
}

let john = Person(name: "John", age: 30, occupation: "Developer")
let yaml = try Yams.dump(object: john)

print(yaml) // "- name: John\n  age: 30\n  occupation: Developer\n"
```

## Глибший розбір

YAML має структуру даних, яка легко читається як людиною, що робить його відмінним вибором для зберігання конфігурацій. Він також підтримує вкладені структури, списки та асоціативні масиви, що дозволяє зберігати будь-який тип даних.

Використовуючи Yams фреймворк, ви можете легко читати та записувати YAML дані з об'єктів Swift, що робить його дуже корисним для багатьох проектів.

## Дивіться також

- [Офіційна документація Yams](https://github.com/jpsim/Yams)
- [Інструкція по встановленню Cocoapods](https://cocoapods.org/)
- [Інструкція по встановленню Swift Package Manager](https://swift.org/package-manager/)