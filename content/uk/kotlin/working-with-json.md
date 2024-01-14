---
title:                "Kotlin: Робота з json"
simple_title:         "Робота з json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

Створення програм, які використовують дані у форматі JSON, є необхідною частиною розробки в сучасному світі. Цей формат дає можливість зручного та ефективного обміну даними між різними системами та платформами. Для багатьох мов програмування, зокрема Kotlin, є інструменти для роботи з JSON, що спрощують процес його обробки.

## Як

Для роботи з JSON у Kotlin потрібно використовувати сторонню бібліотеку, наприклад, Gson. Почнемо з додавання її до нашого проекту. У файлі `build.gradle` додайте наступне:

```Kotlin
dependencies {
    implementation 'com.google.code.gson:gson:2.8.8'
}
```

Тепер можна починати парсити та обробляти дані у форматі JSON. Наприклад, якщо ми маємо такий рядок з даними у форматі JSON:

```Kotlin
val json = """{"name": "Марія", "age": 25, "country": "Україна"}"""
```

Можна створити модель, яка відповідатиме цим даним:

```Kotlin
data class User(val name: String, val age: Int, val country: String)
```

Потім, за допомогою Gson, ми можемо зробити парсинг цього рядка у об'єкт `User`:

```Kotlin
val user = Gson().fromJson(json, User::class.java)
println(user.name) // виведе: Марія
println(user.age) // виведе: 25
println(user.country) // виведе: Україна
```

Це лише один з багатьох способів роботи з JSON в Kotlin за допомогою бібліотеки Gson. Детальніше про її функціонал та можливості можна дізнатися на [офіційному сайті](https://github.com/google/gson).

## Більш детально

Парсинг та обробка даних у форматі JSON може бути дещо складнішою задачею, ніж просто отримання простих значень. Для цього можна використовувати різні стратегії та додаткові налаштування бібліотеки Gson. Також можуть виникнути проблеми з неправильним форматом даних, який значно утруднює їх обробку. Якщо у вас виникають складнощі з роботою з JSON у Kotlin, рекомендуємо глибше зазирнути в документацію бібліотеки та сформулювати питання в спільноті Kotlin.

## Бачили також

- [Офіційна документація Kotlin](https://kotlinlang.org/docs/home.html)
- [Офіційний репозиторій бібліотеки Gson](https://github.com/google/gson)
- [Стаття про роботу з JSON у Kotlin](https://www.raywenderlich.com/5470-json-tutorial-for-beginners-learning-json-using-kotlin)