---
title:                "Kotlin: Запис до стандартного виводу помилок"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Чому
Існує багато різних причин, чому люди можуть прагнути писати до стандартного виводу помилок у Kotlin. Найчастіше те, що пишеться до стандартного виводу помилок, допомагає відловити та виправити збої та помилки у програмі, що дає більш надійну та стабільну роботу.

## Як це зробити
Існує декілька способів, якими можна писати до стандартного виводу помилок у Kotlin. Нижче наведені два приклади коду з вихідними даними для демонстрації:

```Kotlin
// Приклад 1: Використання `System.err.println()`
fun main() {
   val number = "abc".toInt()
   System.err.println("Це не дуже добрий спосіб виведення помилки")
}

// Вихідні дані:
// Exception деталі: java.lang.NumberFormatException: For input string: "abc"
// Це не дуже добрий спосіб виведення помилки
```

```Kotlin
// Приклад 2: Використання `Logger` з бібліотеки `Java.Util.Logging`
import java.util.logging.Level
import java.util.logging.Logger

val logger = Logger.getLogger("Main")

fun main() {
    try {
        val result = 10 / 0 // Ділення на нуль для генерації помилки
        logger.log(Level.INFO, "Результат ділення: $result")
    } catch (e: ArithmeticException) {
        logger.log(Level.SEVERE, "Щось пішло не так", e)
    }
}

// Вихідні дані (у вигляді журналу):
// 29 серп. 2021 12:00:00 - INFO - Main - Результат ділення: 2
// 29 серп. 2021 12:00:00 - SEVERE - Main - Щось пішло не так
// java.lang.ArithmeticException: / by zero
// 	at Main.main(Main.kt:7)
```

## Глибокий занурення
Крім виведення помилок, писання до стандартного виводу також може бути корисним для відловлення дебаг-повідомлень та інформаційних повідомлень у великих програмах. Більшість фреймворків та бібліотек мають свої власні засоби для виведення повідомлень до стандартного виводу. Також зазначується, що якесь інше місце для запису помилок (наприклад, файл журналу) може бути більш практичним для деяких проектів.

## Дивіться також
- [Офіційна документація Kotlin по роботі з файлами та потоками](https://kotlinlang.org/docs/idioms/files.html)
- [Компонент логування для Kotlin](https://github.com/rajandm/Kotlin-Logging)
- [Детальніше про використання стандартного виводу у Kotlin](https://www.javacodeexamples.com/kotlin-tutorial-write-exception-to-standard-error-stream/877)