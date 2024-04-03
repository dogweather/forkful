---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:13.700155-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0434\u0435\u043B\u0430\u0435\
  \u0442\u0441\u044F: \u0414\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441\
  \ JSON \u0432 Kotlin \u043C\u043E\u0436\u043D\u043E \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0431\u0438\u0431\u043B\u0438\u043E\u0442\
  \u0435\u043A\u0443 `kotlinx.serialization`. \u0412\u043E\u0442 \u043F\u0440\u043E\
  \u0441\u0442\u043E\u0439 \u043F\u0440\u0438\u043C\u0435\u0440 \u0441\u0435\u0440\
  \u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438 \u0438 \u0434\u0435\u0441\
  \u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438\u2026"
lastmod: '2024-03-13T22:44:45.018281-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441 JSON \u0432\
  \ Kotlin \u043C\u043E\u0436\u043D\u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u043E\u0432\u0430\u0442\u044C \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\
  \u0443 `kotlinx.serialization`."
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
