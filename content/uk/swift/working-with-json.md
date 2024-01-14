---
title:                "Swift: Робота з json"
simple_title:         "Робота з json"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/swift/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

В сучасному світі програмування дуже важливо володіти такими технологіями, як JSON. Це дозволяє нам зручно обмінюватися та зберігати дані різного формату. Робота з JSON є необхідною навичкою для будь-якого програміста, тому що цей формат є дуже популярним у сфері веб-розробки та інших галузях.

## Як

Для роботи з JSON у Swift нам знадобиться використовувати спеціальний фреймворк - `Foundation`. Це вбудований фреймворк, який надає нам всі необхідні інструменти для роботи з JSON. Давайте розглянемо приклад коду, який демонструє, як розпарсити JSON-рядок та отримати дані з нього:

```Swift
// Приклад JSON-рядка
let jsonString = """
{
    "name" : "John",
    "age" : 25,
    "interests" : ["programming", "photography", "music"]
}
"""

// Розпарсимо JSON та отримаємо дані
do {
    if let json = try JSONSerialization.jsonObject(with: Data(string: jsonString), options: []) as? [String: Any] {
        let name = json["name"] as? String
        let age = json["age"] as? Int
        let interests = json["interests"] as? [String]
        
        print(name, age, interests) // виведе "John", 25, ["programming", "photography", "music"]
    }
} catch {
    print(error)
}
```

Як бачимо, для розпарсингу ми використовуємо метод `JSONSerialization.jsonObject`, який повертає нам об'єкт типу `[String: Any]`, де ключ - це назва параметра, а значення - це уже самі дані. Після цього, ми можемо зручно отримати необхідні дані з цього об'єкта.

## Глибоке дослідження

Розглянутий приклад є досить простим, проте робота з більш складними структурами JSON не відрізняється суттєво. Для кращого розуміння роботи з JSON в Swift, рекомендуємо ознайомитися з документацією та робити більше власних експериментів з цим форматом даних.

## Дивись також

- [Офіційна документація Swift по роботі з JSON](https://developer.apple.com/documentation/foundation/jsonserialization)
- [Стаття "Робота з JSON у Swift" на сайті swiftbook.ru](https://swiftbook.ru/post/tutorials/swift-work-with-json/)
- [Відео "Робота з JSON у Swift" на каналі CodeWithChris](https://www.youtube.com/watch?v=iEKbd5rbmNA)