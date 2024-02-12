---
title:                "Зробити першу літеру рядка великою"
aliases:
- /uk/kotlin/capitalizing-a-string.md
date:                  2024-02-03T19:05:57.310767-07:00
model:                 gpt-4-0125-preview
simple_title:         "Зробити першу літеру рядка великою"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?

Перетворення рядка на початкові великі літери у програмуванні включає конвертацію першого символу рядка в верхній регістр, якщо він вже не є таким, що є корисним для форматування введення користувача або відображення тексту в інтерфейсі користувача більш стандартизованим або дружнім до людини способом. Програмісти виконують цю операцію, щоб забезпечити консистенцію даних або відповідати конкретним вимогам форматування в межах їхніх програмних застосунків.

## Як зробити:

У Kotlin рядки можуть бути перетворені на початкові великі літери за допомогою стандартних бібліотечних функцій без потреби в сторонніх бібліотеках. Підхід Kotlin до обробки рядків робить ці операції простими та лаконічними.

### Перетворення всього рядка:

```kotlin
val message = "hello, world!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // Вивід: HELLO, WORLD!
```

### Перетворення тільки першого символу:

З версії Kotlin 1.5 функція `capitalize()` є застарілою і замінена комбінацією `replaceFirstChar` та лямбда-виразом, який перевіряє, чи є символ малим літером, щоб трансформувати його у верхній регістр.

```kotlin
val greeting = "hello, world!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // Вивід: Hello, world!
```

Цей підхід зберігає решту речення в його оригінальній формі, змінюючи лише першу літеру на велику.
