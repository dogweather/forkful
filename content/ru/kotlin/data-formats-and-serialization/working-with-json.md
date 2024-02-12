---
title:                "Работа с JSON"
aliases:
- /ru/kotlin/working-with-json/
date:                  2024-01-29T00:04:13.700155-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

JSON (JavaScript Object Notation) — это формат структурирования данных, используемый для хранения и передачи. Программисты используют его, потому что он легковесный, удобочитаемый и легко анализируется многими языками, включая Kotlin.

## Как это делается:

Для работы с JSON в Kotlin можно использовать библиотеку `kotlinx.serialization`. Вот простой пример сериализации и десериализации класса данных.

```Kotlin
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import kotlinx.serialization.encodeToString
import kotlinx.serialization.decodeFromString

@Serializable
data class User(val name: String, val age: Int)

fun main() {
    val json = Json { prettyPrint = true }
    val userData = User("Джон Доу", 30)
    
    // Сериализация в JSON
    val jsonString = json.encodeToString(userData)
    println(jsonString)
    
    // Десериализация из JSON
    val userObj = json.decodeFromString<User>(jsonString)
    println(userObj)
}
```

Пример вывода:

```
{
    "name": "Джон Доу",
    "age": 30
}
User(name=Джон Доу, age=30)
```

## Глубже

Простой синтаксис JSON имеет корни в JavaScript, но теперь он независим от языка. Альтернативы, такие как XML, более многословны. При работе с JSON в Kotlin библиотека `kotlinx.serialization` берет на себя основную работу, автоматически преобразуя объекты Kotlin в JSON и обратно с помощью аннотаций. Она поддерживает сложные типы данных и обрабатывает особые случаи, но если вам нужен более тесный контроль, ручной анализ JSON тоже является вариантом.

## Смотрите также

- Руководство по сериализации Kotlin: [https://kotlinlang.org/docs/serialization.html](https://kotlinlang.org/docs/serialization.html)
- Введение в JSON: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
