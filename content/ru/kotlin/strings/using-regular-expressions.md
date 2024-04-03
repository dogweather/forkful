---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:03.771241-07:00
description: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0435 \u0432\u044B\
  \u0440\u0430\u0436\u0435\u043D\u0438\u044F (regex) \u2014 \u044D\u0442\u043E \u0438\
  \u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u044B \u0434\u043B\u044F\
  \ \u043F\u043E\u0438\u0441\u043A\u0430 \u0448\u0430\u0431\u043B\u043E\u043D\u043E\
  \u0432 \u0432 \u0442\u0435\u043A\u0441\u0442\u0435. \u041F\u0440\u043E\u0433\u0440\
  \u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\
  \u0443\u044E\u0442 \u0438\u0445 \u0434\u043B\u044F \u043F\u043E\u0438\u0441\u043A\
  \u0430, \u0432\u0430\u043B\u0438\u0434\u0430\u0446\u0438\u0438 \u0438\u043B\u0438\
  \ \u043C\u0430\u043D\u0438\u043F\u0443\u043B\u044F\u0446\u0438\u0438 \u0434\u0430\
  \u043D\u043D\u044B\u043C\u0438\u2026"
lastmod: '2024-03-13T22:44:44.959705-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0435 \u0432\u044B\
  \u0440\u0430\u0436\u0435\u043D\u0438\u044F (regex) \u2014 \u044D\u0442\u043E \u0438\
  \u043D\u0441\u0442\u0440\u0443\u043C\u0435\u043D\u0442\u044B \u0434\u043B\u044F\
  \ \u043F\u043E\u0438\u0441\u043A\u0430 \u0448\u0430\u0431\u043B\u043E\u043D\u043E\
  \u0432 \u0432 \u0442\u0435\u043A\u0441\u0442\u0435."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0445 \u0432\u044B\u0440\
  \u0430\u0436\u0435\u043D\u0438\u0439"
weight: 11
---

## Как использовать:
Kotlin упрощает работу с regex. Давайте рассмотрим несколько практических примеров кода:

```Kotlin
fun regexFind() {
    val pattern = "Kotlin".toRegex()
    val text = "Учить Kotlin весело!"
    val matchResult = pattern.find(text)
    println(matchResult?.value) // Вывод: Kotlin
}

fun regexReplace() {
    val regex = "\\d+".toRegex()
    val address = "123 Главная Улица"
    val sanitizedAddress = regex.replace(address, "###")
    println(sanitizedAddress) // Вывод: ### Главная Улица
}

fun regexValidate() {
    val passwordPattern = "^(?=.*[A-Za-z])(?=.*\\d)[A-Za-z\\d]{8,}$".toRegex()
    val password = "Password123"
    val isPasswordValid = passwordPattern.matches(password)
    println(isPasswordValid) // Вывод: true
}

regexFind()
regexReplace()
regexValidate()
```

## Глубже в тему
Регулярные выражения являются основным инструментом в программировании с 1950-х годов, изобретены математиком Стивеном Клини. Альтернативы regex включают методы строк, такие как `contains`, `startsWith` или `split`, но они менее мощные. Regex в Kotlin построен на основе классов `Pattern` и `Matcher` из Java, обеспечивая его надежную производительность и утилиту.

## Смотрите также
- Документация Kotlin по Regex: [kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- Инструмент для тестирования Regex: [regex101.com](https://regex101.com/)
- Учебник по Regex: [regular-expressions.info](https://www.regular-expressions.info/tutorial.html)
