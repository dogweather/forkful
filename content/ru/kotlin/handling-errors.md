---
title:                "Обработка ошибок"
date:                  2024-01-28T23:59:04.309300-07:00
model:                 gpt-4-0125-preview
simple_title:         "Обработка ошибок"

category:             "Kotlin"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/kotlin/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Обработка ошибок — это способ, которым ваш код справляется с проблемами, возникающими во время выполнения — как ловля неожиданного мяча без его уронить. Программисты делают это, чтобы предотвратить сбои и обеспечить пользователям плавный опыт использования.

## Как это сделать:
Kotlin предоставляет `try`, `catch`, `finally` и `throw` для управления ошибками. Вот как их использовать:

```Kotlin
fun main() {
    val numerator = 10
    val denominator = 0

    try {
        val result = numerator / denominator
        println("Результат: $result")
    } catch (e: ArithmeticException) {
        println("На ноль делить нельзя, дружище.")
    } finally {
        println("Это происходит в любом случае.")
    }
}
```

Вывод:
```
На ноль делить нельзя, дружище.
Это происходит в любом случае.
```

Если что-то идет не так в блоке `try`, выполнение переходит к `catch`. Он ловит конкретную ошибку, которая была выброшена (`ArithmeticException` в данном случае). Блок `finally` выполняется после — независимо от исхода.

## Подробнее
Блок `try-catch` существует с ранних дней программирования — это как сеть безопасности. Kotlin также предлагает `throw` для ручного выброса исключения в арену, и есть `finally` для кода, который должен выполняться — часто это работа по очистке.

Альтернативы включают в себя тип `Result` и `try` Kotlin как выражение.

```Kotlin
val result: Result<Int> = try {
    Result.success(numerator / denominator)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
Этот подход возвращает объект `Result` — вы получаете либо успех, либо неудачу без драмы необработанного исключения.

Реализация в Kotlin удобна, потому что вы можете использовать `try` как выражение, что означает, что оно возвращает значение. Такие возможности делают обработку ошибок в Kotlin довольно универсальной. Речь идет о выборе подходящего инструмента для работы, как и в мастерской.

## Смотрите также
- Документация Kotlin по исключениям: [Обработка исключений в Kotlin](https://kotlinlang.org/docs/exception-handling.html)
- Документация типа `Result` в Kotlin: [Kotlin Result](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- "Эффективный Java", 3-е издание, Джошуа Блох — отличные представления об исключениях, хотя и специфично для Java.
