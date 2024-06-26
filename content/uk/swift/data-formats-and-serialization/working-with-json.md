---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:27.997983-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: Swift\
  \ \u0440\u043E\u0431\u0438\u0442\u044C \u0430\u043D\u0430\u043B\u0456\u0437 JSON\
  \ \u043F\u0440\u044F\u043C\u043E\u043B\u0456\u043D\u0456\u0439\u043D\u0438\u043C\
  \ \u0456\u0437 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\
  \u044F\u043C \u043F\u0440\u043E\u0442\u043E\u043A\u043E\u043B\u0443 `Codable`. \u041E\
  \u0441\u044C \u044F\u043A \u043C\u043E\u0436\u043D\u0430 \u0434\u0435\u043A\u043E\
  \u0434\u0443\u0432\u0430\u0442\u0438 JSON \u0434\u043E \u043E\u0431'\u0454\u043A\
  \u0442\u0430 Swift."
lastmod: '2024-03-13T22:44:49.955938-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0440\u043E\u0431\u0438\u0442\u044C \u0430\u043D\u0430\u043B\u0456\
  \u0437 JSON \u043F\u0440\u044F\u043C\u043E\u043B\u0456\u043D\u0456\u0439\u043D\u0438\
  \u043C \u0456\u0437 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\
  \u043D\u044F\u043C \u043F\u0440\u043E\u0442\u043E\u043A\u043E\u043B\u0443 `Codable`."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

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
