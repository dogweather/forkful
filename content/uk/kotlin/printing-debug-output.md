---
title:                "Вивід відлагодження на друк"
html_title:           "Kotlin: Вивід відлагодження на друк"
simple_title:         "Вивід відлагодження на друк"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Чому

Ви хочете бачити, що відбувається на кожному кроці вашої програми та виявляти помилки? Друкування налагоджувального виводу є одним з найкращих способів зробити це у Котлін.

## Як

Код для виведення рядка в консоль:

```Kotlin
println("Текст для виведення")
```

Виведе наступне:

```
Текст для виведення
```

Щоб вивести значення змінної, використовуйте наступний код:

```Kotlin
val number = 10
println("Значення змінної number: $number")
```

Виведе наступне:

```
Значення змінної number: 10
```

## Глибокий погляд

Крім друкування змінних, ви також можете використовувати функцію `debug` для виведення детальної інформації про об'єкт. Наприклад:

```Kotlin
data class User(val name: String, val age: Int)

val user = User("Василь", 25)
debug(user)
```

Виведе наступне:

```
User(name=Василь, age=25)
```

Цей спосіб дуже зручний для відлагодження складних або багатократно використовуваних функцій.

## Дивіться також

- [Змінні та типи даних у Котлін](https://kotlinlang.org/docs/reference/basic-types.html)
- [Введення у Котлін](https://try.kotlinlang.org/)