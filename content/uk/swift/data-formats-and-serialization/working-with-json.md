---
title:                "Робота з JSON"
date:                  2024-02-03T19:24:27.997983-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?

Робота з JSON у Swift означає роботу з легковісним форматом даних для обміну даними. Програмісти використовують JSON для передачі даних між сервером і веб-додатком, оскільки він зрозумілий і легко аналізується людьми та машинами.

## Як це робити:

Swift робить аналіз JSON прямолінійним із використанням протоколу `Codable`. Ось як можна декодувати JSON до об'єкта Swift:

```Swift
import Foundation

// Визначаємо модель, яка відповідає протоколу Codable
struct User: Codable {
    var name: String
    var age: Int
}

// Рядок JSON
let jsonString = """
{
    "name": "Джон Доу",
    "age": 30
}
"""

// Перетворюємо рядок JSON на Data
if let jsonData = jsonString.data(using: .utf8) {
    // Декодуємо дані JSON до об'єкта User
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("Ім'я: \(user.name), Вік: \(user.age)")
    } catch {
        print("Помилка декодування JSON: \(error)")
    }
}
```

Приклад виводу:
```
Ім'я: Джон Доу, Вік: 30
```

## Глибше занурення

JSON (JavaScript Object Notation) широко використовується з початку 2000-х років, після того як Дуглас Крокфорд його визначив. Він замінив XML у багатьох випадках завдяки своєму простішому синтаксису та кращій продуктивності. Хоча `Codable` в Swift є головним для роботи з JSON, існують альтернативи, такі як `JSONSerialization`, коли потрібно працювати з типами, що не підпадають під протокол Codable. За лаштунками `Codable` абстрагує низькорівневий аналіз і робить серіалізацію/десеріалізацію безшовною.

## Дивіться також

- Дізнайтеся більше про JSON і Swift на офіційному блозі Swift: [Swift.org](https://swift.org/blog/)
- Ознайомтеся з документацією `Codable`: [Swift Codable](https://developer.apple.com/documentation/swift/codable)
- Для складних структур JSON розгляньте можливість використання сторонніх бібліотек, таких як SwiftyJSON, доступних на [GitHub](https://github.com/SwiftyJSON/SwiftyJSON).
