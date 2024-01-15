---
title:                "Робота з форматом JSON"
html_title:           "Kotlin: Робота з форматом JSON"
simple_title:         "Робота з форматом JSON"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

Користувачі Kotlin часто працюють з JSON для обміну даними з іншими додатками або зберігання конфігурацій. Використання Kotlin дозволяє зручно та ефективно працювати з цим форматом даних.

## Як

```Kotlin
// Приклад створення JSON об'єкту
val person = JsonObject()
person.addProperty("name", "John Doe")
person.addProperty("age", 25)
// Результат: {"name":"John Doe","age":25}

// Приклад отримання значення з JSON об'єкту
val name = person.get("name").asString
// Результат: "John Doe"

// Приклад розбору JSON рядка
val json = "{\"name\":\"Jane Smith\",\"age\":30}"
val person = JsonParser().parse(json).asJsonObject
val name = person.get("name").asString
val age = person.get("age").asInt
// Результат: name = "Jane Smith", age = 30
```

### Deep Dive

Kotlin має вбудовану підтримку для роботи з JSON за допомогою бібліотеки Gson. Вона дозволяє легко створювати та розбирати об'єкти JSON, а також використовувати анотації для подальшої обробки даних.

## See Also

- [Документація по роботі з JSON в Kotlin](https://kotlinlang.org/docs/reference/whatsnew12.html#json-serialization) 
- [Корисні поради для роботи з Kotlin та JSON](https://blog.kotlin-academy.com/parsing-json-in-kotlin-using-gson-94c5aaf8584e)