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

## Чому

У сучасному програмуванні, є безліч форматів для обміну даними між додатками. Один із них, JSON, є дуже поширеним завдяки своїй простоті та легкості використання. Розуміння того, як працювати з JSON, дозволить зберігати та обмінюватися даними з різних джерел без зайвих зусиль.

## Як працювати з JSON у Swift

Існує кілька способів обробки даних у форматі JSON у Swift, але найбільш простим і найпопулярнішим є використання стандартної бібліотеки Foundation.

Наприклад, якщо ми маємо JSON-файл з даними про користувачів, ми можемо здійснити його парсинг та отримати вміст у вигляді словника Swift за допомогою наступного коду:

```Swift 
if let path = Bundle.main.path(forResource: "users", ofType: "json") {
    do {
        let data = try Data(contentsOf: URL(fileURLWithPath: path), options: .mappedIfSafe)
        let users = try JSONSerialization.jsonObject(with: data, options: .allowFragments) as? [String: Any]
        print(users)
    } catch {
        print(error)
    }
}
```

Результат виконання цього коду буде виглядати приблизно так:

```
Optional(["users": <__NSArrayM 0x600002462e10>(
{
    "name" = "John";
    "age" = 25;
},
{
    "name" = "Amanda";
    "age" = 30;
},
{
    "name" = "Mark";
    "age" = 20;
}
)
])
```

Тут ми звернулися до файлу "users.json", який містить список користувачів та їх вік. Зчитавши цей файл із допомогою методу `Data(contentsOf:options:)` та використавши метод `jsonObject(with:options:)`, ми отримали словник у форматі [String: Any]. Далі, ми можемо легко отримати доступ до конкретної інформації, як наприклад, ім'я першого користувача: `users["users"]?[0]["name"]`.

## Детальніше про роботу з JSON у Swift

Методи `Data(contentsOf:options:)` та `jsonObject(with:options:)` не єдині способи обробки даних у форматі JSON у Swift. Також можна використовувати сторонні бібліотеки, які дозволяють більш зручно та ефективно працювати з JSON-даними.

Наприклад, бібліотека SwiftyJSON дозволяє отримати доступ до JSON-даних за допомогою зручних методів та операцій, зробивши код більш зрозумілим та елегантним. Вона також автоматично переводить типи даних, зменшуючи ризик помилок при роботі з JSON.

Для ознайомлення з іншими бібліотеками та рішеннями для роботи з JSON у Swift, рекоменд