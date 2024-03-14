---
date: 2024-01-20 17:35:32.291438-07:00
description: "\u041E\u0431'\u0454\u0434\u043D\u0430\u043D\u043D\u044F \u0440\u044F\
  \u0434\u043A\u0456\u0432 \u0443 Kotlin \u2014 \u0446\u0435 \u0441\u043F\u043E\u0441\
  \u0456\u0431 \u0437'\u0454\u0434\u043D\u0430\u0442\u0438 \u043A\u0456\u043B\u044C\
  \u043A\u0430 \u0440\u044F\u0434\u043A\u0456\u0432 \u0432 \u043E\u0434\u0438\u043D\
  . \u0426\u0435 \u043A\u043E\u0440\u0438\u0441\u043D\u043E, \u043A\u043E\u043B\u0438\
  \ \u0432\u0430\u043C \u043F\u043E\u0442\u0440\u0456\u0431\u043D\u043E \u0437\u0440\
  \u043E\u0431\u0438\u0442\u0438 \u043F\u043E\u0432\u043D\u0435 \u043F\u043E\u0432\
  \u0456\u0434\u043E\u043C\u043B\u0435\u043D\u043D\u044F \u0447\u0438 \u0441\u0444\
  \u043E\u0440\u043C\u0443\u0432\u0430\u0442\u0438 \u0434\u0438\u043D\u0430\u043C\u0456\
  \u0447\u043D\u0456\u2026"
lastmod: '2024-03-13T22:44:49.205620-06:00'
model: gpt-4-1106-preview
summary: "\u041E\u0431'\u0454\u0434\u043D\u0430\u043D\u043D\u044F \u0440\u044F\u0434\
  \u043A\u0456\u0432 \u0443 Kotlin \u2014 \u0446\u0435 \u0441\u043F\u043E\u0441\u0456\
  \u0431 \u0437'\u0454\u0434\u043D\u0430\u0442\u0438 \u043A\u0456\u043B\u044C\u043A\
  \u0430 \u0440\u044F\u0434\u043A\u0456\u0432 \u0432 \u043E\u0434\u0438\u043D. \u0426\
  \u0435 \u043A\u043E\u0440\u0438\u0441\u043D\u043E, \u043A\u043E\u043B\u0438 \u0432\
  \u0430\u043C \u043F\u043E\u0442\u0440\u0456\u0431\u043D\u043E \u0437\u0440\u043E\
  \u0431\u0438\u0442\u0438 \u043F\u043E\u0432\u043D\u0435 \u043F\u043E\u0432\u0456\
  \u0434\u043E\u043C\u043B\u0435\u043D\u043D\u044F \u0447\u0438 \u0441\u0444\u043E\
  \u0440\u043C\u0443\u0432\u0430\u0442\u0438 \u0434\u0438\u043D\u0430\u043C\u0456\u0447\
  \u043D\u0456\u2026"
title: "\u041E\u0431'\u0454\u0434\u043D\u0430\u043D\u043D\u044F \u0440\u044F\u0434\
  \u043A\u0456\u0432"
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
