---
date: 2024-01-20 17:58:22.145395-07:00
description: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\
  \u043D\u0430 \u0442\u0435\u043A\u0441\u0442\u0443 - \u043F\u0440\u043E\u0446\u0435\
  \u0441 \u0437\u043D\u0430\u0445\u043E\u0434\u0436\u0435\u043D\u043D\u044F \u0440\
  \u044F\u0434\u043A\u0456\u0432 \u0456 \u0457\u0445\u043D\u044C\u043E\u0457 \u043C\
  \u043E\u0434\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0457. \u041F\u0440\u043E\
  \u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0432\u0438\u043A\u043E\u0440\u0438\
  \u0441\u0442\u043E\u0432\u0443\u044E\u0442\u044C \u0446\u0435 \u0434\u043B\u044F\
  \ \u0432\u0438\u043F\u0440\u0430\u0432\u043B\u0435\u043D\u043D\u044F \u043F\u043E\
  \u043C\u0438\u043B\u043E\u043A, \u043E\u043D\u043E\u0432\u043B\u0435\u043D\u043D\
  \u044F \u0434\u0430\u043D\u0438\u0445 \u0447\u0438\u2026"
lastmod: '2024-03-13T22:44:49.195037-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443 - \u043F\u0440\u043E\u0446\u0435\u0441\
  \ \u0437\u043D\u0430\u0445\u043E\u0434\u0436\u0435\u043D\u043D\u044F \u0440\u044F\
  \u0434\u043A\u0456\u0432 \u0456 \u0457\u0445\u043D\u044C\u043E\u0457 \u043C\u043E\
  \u0434\u0438\u0444\u0456\u043A\u0430\u0446\u0456\u0457."
title: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443"
weight: 10
---

## Як це зробити:
```kotlin
fun main() {
    val text = "Шановні, вітаємо вас у світі Kotlin!"
    val searchText = "вас"
    val replaceText = "всіх"

    val updatedText = text.replace(searchText, replaceText)
    println(updatedText)
}

// Вивід: Шановні, вітаємо всіх у світі Kotlin!
```

## Глибший занурення:
Пошук та заміна тексту існує з самого початку комп'ютерного програмування. Раніше робили мануально, зараз можна автоматизувати. В Kotlin є вбудовані методи, наприклад `replace()`, які використовують regex (регулярні вирази) для гнучкості. Альтернативи включають бібліотеки, як Apache Commons Lang в Java. Розуміння регулярних виразів важливе для ефективного пошуку та заміни.

## Дивіться також:
- [Kotlin Official Documentation for the replace function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [RegexOne - Understanding Regular Expressions](https://regexone.com/)
- [Apache Commons Lang StringUtils](https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html)
