---
title:                "Робота з JSON"
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON – це формат обміну даними. Програмісти використовують його для легкого та зручного обміну даними між мережею і програмами. Він простий, легкочитаний для людей та генерується та парситься багатьма мовами програмування.

## How to:
Kotlin працює з JSON легко, користуючись бібліотекою `kotlinx.serialization`. Ось приклад коду:

```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class User(val name: String, val age: Int)

fun main() {
    val json = Json.encodeToString(User("Іван", 30))
    println(json) // Виводить: {"name":"Іван","age":30}

    val user = Json.decodeFromString<User>(json)
    println(user) // Виводить: User(name=Іван, age=30)
}
```

## Deep Dive:
JSON з'явився в 2001 році як альтернатива XML, пропонуючи менш громіздкий формат. В Kotlin, `kotlinx.serialization` є сучасним вибором, але існують інші бібліотеки, як Gson та Moshi. `kotlinx.serialization` використовує анотації для визначення серіалізованих класів, що забезпечує типобезпечне і ефективне перетворення об'єктів у JSON і назад.

## See Also:
- Керівництво по `kotlinx.serialization`: https://kotlinlang.org/docs/serialization.html
- Документація JSON: https://www.json.org/json-uk.html
- Gson бібліотека: https://github.com/google/gson
- Moshi бібліотека: https://github.com/square/moshi