---
title:                "Виведення налагоджувальної інформації"
aliases:
- /uk/kotlin/printing-debug-output/
date:                  2024-01-20T17:52:52.915994-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Вивід дебаг інформації - це коли ми друкуємо дані, щоб зрозуміти, що відбувається в коді. Програмісти це роблять, щоб швидко знайти помилки та перевірити поточний стан змінних.

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
