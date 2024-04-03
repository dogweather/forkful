---
date: 2024-01-20 17:58:22.145395-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : ."
lastmod: '2024-03-13T22:44:49.195037-06:00'
model: gpt-4-1106-preview
summary: .
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
