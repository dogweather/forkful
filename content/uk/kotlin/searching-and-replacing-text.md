---
title:                "Kotlin: Пошук та заміна тексту"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

Програмування може бути важливою і потужною навичкою, яка полегшує роботу. Заміна тексту є однією з найбільш корисних можливостей для програмістів, яка допомогає пришвидшити та полегшити процес кодування.

## Як

Приклади використання методів заміни тексту в мові програмування Kotlin в рамках "```Kotlin ... ```" кодових блоків та результати виводу:

- Заміна старої назви змінної на нову:

```Kotlin
var username = "UkrainianDev"
username = username.replace("UkrainianDev", "KyivCoder")
println(username)

// Output: KyivCoder 
```

- Заміна всіх літер "a" на "e" у рядку:

```Kotlin
val sentence = "Цей документ містить лише літери A."
val result = sentence.replace("a", "e")
println(result)

// Output: Цей документ містить лише літери e. 
```

- Видалення символів пунктуації з рядку:

```Kotlin
val text = "Привіт! Я український програміст."
val cleanText = text.replace("[^A-Za-z0-9 ]".toRegex(), "")
println(cleanText)

// Output: Привіт Я український програміст 
```

## Глибокий погляд

Заміна тексту може бути використана не лише для простих рядків, але й для більш складних структур даних, таких як масиви та колекції. Крім того, Kotlin має багато інших методів для роботи з текстом, які можуть бути корисними для заміни та маніпуляції даними у вашому коді.

## Дивіться також

- [Документація Kotlin: Методи заміни](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/#replace)
- [Туторіал на YouTube про заміну тексту в Kotlin](https://www.youtube.com/watch?v=HcSIMZcow9k)
- [Стаття Medium про основи роботи з Kotlin та заміну тексту](https://medium.com/@UkrainianDev/kotlin-for-beginners-searching-and-replacing-text-1eb0ad4f5a64)