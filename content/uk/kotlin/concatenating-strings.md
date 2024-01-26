---
title:                "Об'єднання рядків"
date:                  2024-01-20T17:35:32.291438-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Об'єднання рядків у Kotlin — це спосіб з'єднати кілька рядків в один. Це корисно, коли вам потрібно зробити повне повідомлення чи сформувати динамічні дані для виводу.

## Як це зробити:
```kotlin
fun main() {
    val greeting = "Привіт"
    val world = "Світ"
    val exclamation = "!"

    // Використання оператора +
    val message1 = greeting + ", " + world + exclamation
    println(message1) // Вивід: Привіт, Світ!

    // String templates (рядкові шаблони)
    val message2 = "$greeting, $world$exclamation"
    println(message2) // Вивід: Привіт, Світ!

    // StringBuilder
    val stringBuilder = StringBuilder()
    stringBuilder.append(greeting)
    stringBuilder.append(", ")
    stringBuilder.append(world)
    stringBuilder.append(exclamation)
    val message3 = stringBuilder.toString()
    println(message3) // Вивід: Привіт, Світ!
}
```

## Поглиблений Розгляд
У минулому, коли ресурси були обмежені, об'єднання рядків могло значно впливати на продуктивність програми. Зараз компілятор Kotlin оптимізує об'єднання рядків, але для великої кількості або складних операцій варто використовувати StringBuilder.

Альтернативи:
- `plus` метод, наприклад `greeting.plus(", ").plus(world).plus(exclamation)`
- Бібліотеки та функції розширення для більш складних варіантів об'єднання.

Інформація про реалізацію: оператор `+` в Kotlin насправді використовує метод `StringBuilder.append` під капотом, коли компілюється до байткоду JVM, що робить це більш ефективним, ніж може здатися на перший погляд.

## Дивіться Також
- [Kotlin Documentation: Strings](https://kotlinlang.org/docs/basic-types.html#string-literals)
