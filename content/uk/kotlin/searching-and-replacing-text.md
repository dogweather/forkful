---
title:                "Пошук та заміна тексту"
date:                  2024-01-20T17:58:22.145395-07:00
model:                 gpt-4-1106-preview
simple_title:         "Пошук та заміна тексту"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Пошук та заміна тексту - процес знаходження рядків і їхньої модифікації. Програмісти використовують це для виправлення помилок, оновлення даних чи швидкого рефакторингу коду.

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
