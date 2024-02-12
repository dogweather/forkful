---
title:                "Використання регулярних виразів"
aliases:
- uk/kotlin/using-regular-expressions.md
date:                  2024-02-03T19:18:22.245758-07:00
model:                 gpt-4-0125-preview
simple_title:         "Використання регулярних виразів"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?

Регулярні вирази (regex) є потужним інструментом для обробки тексту, що дозволяє програмістам шукати, знаходити відповідності і маніпулювати рядками з використанням передових технік зіставлення шаблонів. У Kotlin використання regex допомагає ефективно виконувати складні задачі обробки тексту, такі як валідація, розбір або перетворення, роблячи його незамінним для завдань, що варіюються від простої маніпуляції рядками до складного аналізу тексту.

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
