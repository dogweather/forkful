---
title:                "Використання регулярних виразів"
date:                  2024-01-19
html_title:           "Bash: Використання регулярних виразів"
simple_title:         "Використання регулярних виразів"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Що і Чому?
Регулярні вирази (regex) - це потужний інструмент для обробки тексту, який дозволяє шукати та замінювати текст за складними правилами. Програмісти використовують regex через їх гнучкість і здатність економити час при великих обсягах даних.

## Як це зробити:
```kotlin
fun main() {
    val text = "Контакти: Іван +380(99)123-4567, Олена +380(50)987-6543."
    
    // Знаходимо всі номери телефонів використовуючи regex
    val phoneRegex = "\\+380\\(\\d{2}\\)\\d{3}-\\d{4}".toRegex()
    val phoneNumbers = phoneRegex.findAll(text).map { it.value }.toList()
    
    println("Номери телефонів: $phoneNumbers")
    
    // Заміняємо імена на "Анонім"
    val anonymizedText = text.replace(Regex("[ІО][а-я]+"), "Анонім")
    
    println("Анонімізований текст: $anonymizedText")
}
```
Вивід:
```
Номери телефонів: [+380(99)123-4567, +380(50)987-6543]
Анонімізований текст: Контакти: Анонім +380(99)123-4567, Анонім +380(50)987-6543.
```

## Занурення у деталі:
Регулярні вирази беруть свій початок з теорії формальних мов і автоматів 1950-х років. Існують альтернативи regex, такі як рядкові функції (indexOf, substring, etc.), наявні в більшості мов програмування, але вони зазвичай менш потужні для складних завдань. Regex у Kotlin реалізований за допомогою класів Pattern і Matcher з Java, що забезпечує високу сумісність та продуктивність.

## Дивіться також:
- [Документація Kotlin по регулярним виразам](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Java Pattern Class](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
- [RegexOne: Інтерактивні уроки регулярних виразів](https://regexone.com/)
