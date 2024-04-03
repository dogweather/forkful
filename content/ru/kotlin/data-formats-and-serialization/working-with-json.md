---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:13.700155-07:00
description: "JSON (JavaScript Object Notation) \u2014 \u044D\u0442\u043E \u0444\u043E\
  \u0440\u043C\u0430\u0442 \u0441\u0442\u0440\u0443\u043A\u0442\u0443\u0440\u0438\u0440\
  \u043E\u0432\u0430\u043D\u0438\u044F \u0434\u0430\u043D\u043D\u044B\u0445, \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C\u044B\u0439 \u0434\u043B\u044F\
  \ \u0445\u0440\u0430\u043D\u0435\u043D\u0438\u044F \u0438 \u043F\u0435\u0440\u0435\
  \u0434\u0430\u0447\u0438. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\
  \u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u044E\u0442\
  \ \u0435\u0433\u043E, \u043F\u043E\u0442\u043E\u043C\u0443 \u0447\u0442\u043E \u043E\
  \u043D\u2026"
lastmod: '2024-03-13T22:44:45.018281-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \u2014 \u044D\u0442\u043E \u0444\u043E\
  \u0440\u043C\u0430\u0442 \u0441\u0442\u0440\u0443\u043A\u0442\u0443\u0440\u0438\u0440\
  \u043E\u0432\u0430\u043D\u0438\u044F \u0434\u0430\u043D\u043D\u044B\u0445, \u0438\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C\u044B\u0439 \u0434\u043B\u044F\
  \ \u0445\u0440\u0430\u043D\u0435\u043D\u0438\u044F \u0438 \u043F\u0435\u0440\u0435\
  \u0434\u0430\u0447\u0438."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

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
