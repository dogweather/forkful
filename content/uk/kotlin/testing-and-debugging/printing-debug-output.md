---
date: 2024-01-20 17:52:52.915994-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412\u0438\u0445\u043E\u0434\u0438\u0442\u044C."
lastmod: '2024-04-05T21:53:49.421351-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043D\u0430\u043B\u0430\
  \u0433\u043E\u0434\u0436\u0443\u0432\u0430\u043B\u044C\u043D\u043E\u0457 \u0456\u043D\
  \u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457"
weight: 33
---

## Як це зробити:
```Kotlin
fun main() {
    val debugMessage = "Привіт, Debug!"
    println(debugMessage)  // Простий вивід в консоль

    val a = 5
    val b = 10
    println("Переменная a = $a, b = $b")  // Вивід з інтерполяцією

    if (a + b > 10) {
        println("Сума більша за 10")  // Умовний вивід
    }
}
```
Виходить:
```
Привіт, Debug!
Переменная a = 5, b = 10
Сума більша за 10
```

## Поглиблений Розбір:
Спочатку, було просто `print()`. А потім з'явився `println()`, який додавав новий рядок після виводу. 

Альтернативи? Журналювання (logging) — більш потужний механізм із підтримкою рівнів(debug, info, warn, error).

Щодо реалізації, `println()` в Kotlin використовує Java's System.out.println(). Можна керувати виведенням, направляючи його в файл чи інше місце замість консолі.

## Дивись Також:
- [Kotlin Logging](https://github.com/MicroUtils/kotlin-logging): легка бібліотека для журналювання.
- [Logging in Kotlin](https://www.baeldung.com/kotlin/logging): гід з прикладами використання різних логінг бібліотек в Kotlin.
- [Official Kotlin Documentation](https://kotlinlang.org/docs/home.html): Документація від розробників Kotlin.
