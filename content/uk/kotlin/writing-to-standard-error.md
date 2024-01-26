---
title:                "Запис в стандартний потік помилок"
html_title:           "Arduino: Запис в стандартний потік помилок"
simple_title:         "Запис в стандартний потік помилок"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Що це та навіщо?
Стандартний потік помилок (stderr) – це канал, через який ваша програма може повідомляти про проблеми. Програмісти використовують stderr, щоб розділити звичайний вивід програми та повідомлення про помилки, спрощуючи діагностику і налагодження.

## Як це зробити:
В Kotlin писати до stderr легко – використовуйте `System.err`:

```Kotlin
fun main() {
    System.err.println("Це повідомлення помилки")
}

```
Якщо запустити таку програму, у консолі ви побачите:

```
Це повідомлення помилки
```

## Поглиблений огляд:
У багатьох операційних системах stderr вводили з тією метою, щоб дозволити програмам відправляти вивід та помилки в різні місця. Це може бути корисним для логування або обробки помилок за допомогою інших програм. Альтернативно, можна використовувати логгери, які мають більше можливостей, але для простих сценаріїв `System.err` буде достатньо. Цей механізм імплементовано в Java Virtual Machine (JVM), і Kotlin як JVM-мова успадковує цей функціонал.

## Дивіться також:
- Oracle's Java Documentation on `System.err`: https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err
- Kotlin Documentation: https://kotlinlang.org/api/latest/jvm/stdlib/
- Effective Logging Practices: https://stackify.com/effective-logging-strategy/  

Переконайтесь, що використовуєте цю функцію обачно – правильне логування може значно полегшити налагодження вашого коду.
