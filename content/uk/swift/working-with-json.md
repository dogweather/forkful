---
title:                "Робота з json"
html_title:           "Swift: Робота з json"
simple_title:         "Робота з json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Що & Чому?
Робота з JSON - це процес, який дозволяє програмі отримувати та обробляти дані у форматі JSON (JavaScript Object Notation). Програмісти використовують цей формат для передачі та отримання даних з веб-серверів або для збереження даних на своєму пристрої.

## Як те робити:
Давайте розглянемо невеликий приклад, як ми можемо використовувати JSON у нашому коді Swift:

```Swift
let jsonData = """
{
    "name": "John",
    "age": 32,
    "city": "Kyiv"
}
""".data(using: .utf8)

struct Person: Codable {
    let name: String
    let age: Int
    let city: String
}

do {
    let person = try JSONDecoder().decode(Person.self, from: jsonData!)
    print(person) // Виведе: Person(name: "John", age: 32, city: "Kyiv")
} catch {
    print(error.localizedDescription) // Обробка помилки, якщо дані JSON неможливо розкодувати
}
```

## Занурення:
JSON був створений у 2001 році як легкий та ефективний формат для обміну даними. Його популярність зросла з розвитком сучасних веб-технологій та мобільного програмування. Існують інші альтернативи JSON, такі як XML та CSV, але JSON залишається найбільш популярним сьогодні.

Розбір та створення JSON-даних у Swift здійснюється з використанням протоколу `Codable`, який дозволяє миттєво перетворювати структури даних у формат JSON та навпаки. Також у Swift є багато сторонніх бібліотек, які роблять роботу з JSON ще простішою та зручнішою.

## Дивіться також:
- [Apple's documentation on Codable](https://developer.apple.com/documentation/foundation/archives_and_serialization/encoding_and_decoding_custom_types)
- [SwiftyJSON - a popular third-party library for working with JSON in Swift](https://github.com/SwiftyJSON/SwiftyJSON)
- [Hacking with Swift's tutorial on working with JSON in Swift](https://www.hackingwithswift.com/read/7/2/working-with-json-in-swift)