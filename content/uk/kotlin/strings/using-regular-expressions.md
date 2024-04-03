---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:22.245758-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : #."
lastmod: '2024-03-13T22:44:49.203295-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u0412\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u0430\u043D\u043D\u044F \u0440\
  \u0435\u0433\u0443\u043B\u044F\u0440\u043D\u0438\u0445 \u0432\u0438\u0440\u0430\u0437\
  \u0456\u0432"
weight: 11
---

## Як це зробити:


### Базове зіставлення
Щоб перевірити, чи відповідає рядок певному шаблону в Kotlin, можна скористатися методом `matches` класу `Regex`.

```kotlin
val pattern = "kotlin".toRegex()
val input = "I love kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // Вивід: true
```

### Знаходження та видобування частин рядка
Якщо ви хочете знайти частини рядка, які відповідають шаблону, Kotlin дозволяє ітерувати по всім відповідностям:

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "Сьогоднішня дата 07/09/2023."
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// Вивід: 07/09/2023
```

### Заміна тексту
Замінити частини рядка, які відповідають певному шаблону, легко з функцією `replace`:

```kotlin
val input = "Username: user123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // Вивід: Username: userXXX
```

### Розбиття рядків
Розбийте рядок на список, використовуючи шаблон regex як роздільник:

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // Вивід: [1, 2, 3, 4, 5]
```

### Бібліотеки сторонніх розробників: Kotest
[Kotest](https://github.com/kotest/kotest) — популярна бібліотека тестування для Kotlin, яка розширює вбудовану підтримку regex в Kotlin, особливо корисна для валідації у тестових випадках.

```kotlin
// Припускаючи, що Kotest додано до вашого проекту
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@test.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// Це пройде тест, якщо вхідні дані відповідають шаблону електронної пошти.
```

Інтегруючи регулярні вирази у свої додатки Kotlin, ви можете ефективно виконувати складну обробку тексту. Незалежно від того, чи валідуєте ви вхідні дані користувача, видобуваєте дані або перетворюєте рядки, шаблони regex пропонують надійне рішення.
