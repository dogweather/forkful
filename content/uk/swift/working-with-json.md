---
title:                "Робота з JSON"
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?

JSON (JavaScript Object Notation) — це формат обміну даними. Програмісти використовують його через його простоту та широку підтримку у різних мовах програмування, включаючи Swift, для зберігання даних та взаємодії з серверами.

## Як це зробити:

```Swift
import Foundation

// Дані JSON в форматі строки
let jsonString = """
{
    "name": "Володимир",
    "age": 30,
    "isDeveloper": true
}
"""

// Структура для відображення даних JSON
struct User: Codable {
    var name: String
    var age: Int
    var isDeveloper: Bool
}

// Парсинг JSON
if let jsonData = jsonString.data(using: .utf8) {
    do {
        let user = try JSONDecoder().decode(User.self, from: jsonData)
        print("Ім'я: \(user.name), Вік: \(user.age), Розробник: \(user.isDeveloper)")
    } catch {
        print("Помилка при парсингу: \(error)")
    }
}
```

Вивід:
```
Ім'я: Володимир, Вік: 30, Розробник: true
```

## Глибинне занурення:

JSON вперше був представлений Дугласом Крокфордом у 2001 році. Його легкість та зрозумілість зробили його популярним варіантом для взаємодії між клієнтом та сервером, а також для зберігання даних. Альтернативами JSON є XML та YAML, але вони помітно важчі для читання та написання. В Swift з використанням `Codable` протоколу реалізовано зручну серіалізацію та десеріалізацію JSON, що робить його обробку ефективною та інтуїтивною.

## Ще трохи інформації:

- [raywenderlich.com - повне керівництво по Codable у Swift](https://www.raywenderlich.com/3418439-encoding-and-decoding-in-swift)
- [Apple Developer - Foundation's JSONSerialization](https://developer.apple.com/documentation/foundation/jsonserialization)
- [JSON.org - специфікація JSON](http://json.org/json-uk.html)