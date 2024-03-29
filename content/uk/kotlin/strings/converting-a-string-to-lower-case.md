---
date: 2024-01-20 17:39:18.318832-07:00
description: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0440\u044F\u0434\u043A\u0456\u0432 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439\
  \ \u0440\u0435\u0433\u0456\u0441\u0442\u0440 - \u0446\u0435 \u043F\u0440\u043E\u0446\
  \u0435\u0441 \u0437\u043C\u0456\u043D\u0438 \u0443\u0441\u0456\u0445 \u043B\u0456\
  \u0442\u0435\u0440 \u0443 \u0440\u044F\u0434\u043A\u0443 \u043D\u0430 \u0457\u0445\
  \ \u0435\u043A\u0432\u0456\u0432\u0430\u043B\u0435\u043D\u0442\u0438 \u0443 \u043D\
  \u0438\u0436\u043D\u044C\u043E\u043C\u0443 \u0440\u0435\u0433\u0456\u0441\u0442\u0440\
  \u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\
  \u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u0443\u043D\
  \u0456\u0444\u0456\u043A\u0430\u0446\u0456\u0457\u2026"
lastmod: '2024-03-13T22:44:49.198241-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F\
  \ \u0440\u044F\u0434\u043A\u0456\u0432 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439\
  \ \u0440\u0435\u0433\u0456\u0441\u0442\u0440 - \u0446\u0435 \u043F\u0440\u043E\u0446\
  \u0435\u0441 \u0437\u043C\u0456\u043D\u0438 \u0443\u0441\u0456\u0445 \u043B\u0456\
  \u0442\u0435\u0440 \u0443 \u0440\u044F\u0434\u043A\u0443 \u043D\u0430 \u0457\u0445\
  \ \u0435\u043A\u0432\u0456\u0432\u0430\u043B\u0435\u043D\u0442\u0438 \u0443 \u043D\
  \u0438\u0436\u043D\u044C\u043E\u043C\u0443 \u0440\u0435\u0433\u0456\u0441\u0442\u0440\
  \u0456. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\u0438 \u0440\
  \u043E\u0431\u043B\u044F\u0442\u044C \u0446\u0435 \u0434\u043B\u044F \u0443\u043D\
  \u0456\u0444\u0456\u043A\u0430\u0446\u0456\u0457\u2026"
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\
  \u044F\u0434\u043A\u0430 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\u0435\
  \u0433\u0456\u0441\u0442\u0440"
---

{{< edit_this_page >}}

## Що і Чому?
Перетворення рядків у нижній регістр - це процес зміни усіх літер у рядку на їх еквіваленти у нижньому регістрі. Програмісти роблять це для уніфікації текстових даних, зокрема при порівнянні рядків чи забезпеченні велик-малої незалежності.

## Як це зробити:
```Kotlin
fun main() {
    val original = "Привіт, Як справи?"
    val lowerCased = original.lowercase()
    println(lowerCased)  // "привіт, як справи?"
}
```
Цей код виведе всі слова змінної `original` у нижньому регістрі.

## Підводні Камені:
На початку доби програмування, коли все було значно примітивнішим, такі дії як перетворення рядків у нижній регістр, здійснювали ручним проходженням символів і використанням таблиць ASCII.

Згодом, з приходом Unicode, справа ускладнилася через багатомовність і різні правила написання символів. Розглядаючи не лише англійську, а й українську та інші мови, Kotlin (і більшість мов програмування) використовують Unicode бібліотеки, що забезпечують коректне перетворення, враховуючи локальні особливості.

Існують також альтернативні методи, такі як `.toUpperCase()`, що заміняє всі літери на верхній регістр, і `capitalize()` для заголовків, де необхідно підняти тільки першу літеру речення.

## Також Подивіться:
- Документацію Kotlin API щодо `String`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/
- Unicode Consortium для детального розуміння Unicode: http://unicode.org/
- Порівняння регістрів у різних програмувальних мовах: https://en.wikipedia.org/wiki/String_(computer_science)#String_operations
