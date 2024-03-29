---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:04.309300-07:00
description: "\u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0430 \u043E\u0448\u0438\
  \u0431\u043E\u043A \u2014 \u044D\u0442\u043E \u0441\u043F\u043E\u0441\u043E\u0431\
  , \u043A\u043E\u0442\u043E\u0440\u044B\u043C \u0432\u0430\u0448 \u043A\u043E\u0434\
  \ \u0441\u043F\u0440\u0430\u0432\u043B\u044F\u0435\u0442\u0441\u044F \u0441 \u043F\
  \u0440\u043E\u0431\u043B\u0435\u043C\u0430\u043C\u0438, \u0432\u043E\u0437\u043D\
  \u0438\u043A\u0430\u044E\u0449\u0438\u043C\u0438 \u0432\u043E \u0432\u0440\u0435\
  \u043C\u044F \u0432\u044B\u043F\u043E\u043B\u043D\u0435\u043D\u0438\u044F \u2014\
  \ \u043A\u0430\u043A \u043B\u043E\u0432\u043B\u044F \u043D\u0435\u043E\u0436\u0438\
  \u0434\u0430\u043D\u043D\u043E\u0433\u043E \u043C\u044F\u0447\u0430 \u0431\u0435\
  \u0437 \u0435\u0433\u043E \u0443\u0440\u043E\u043D\u0438\u0442\u044C.\u2026"
lastmod: '2024-03-13T22:44:44.992699-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0430 \u043E\u0448\u0438\
  \u0431\u043E\u043A \u2014 \u044D\u0442\u043E \u0441\u043F\u043E\u0441\u043E\u0431\
  , \u043A\u043E\u0442\u043E\u0440\u044B\u043C \u0432\u0430\u0448 \u043A\u043E\u0434\
  \ \u0441\u043F\u0440\u0430\u0432\u043B\u044F\u0435\u0442\u0441\u044F \u0441 \u043F\
  \u0440\u043E\u0431\u043B\u0435\u043C\u0430\u043C\u0438, \u0432\u043E\u0437\u043D\
  \u0438\u043A\u0430\u044E\u0449\u0438\u043C\u0438 \u0432\u043E \u0432\u0440\u0435\
  \u043C\u044F \u0432\u044B\u043F\u043E\u043B\u043D\u0435\u043D\u0438\u044F \u2014\
  \ \u043A\u0430\u043A \u043B\u043E\u0432\u043B\u044F \u043D\u0435\u043E\u0436\u0438\
  \u0434\u0430\u043D\u043D\u043E\u0433\u043E \u043C\u044F\u0447\u0430 \u0431\u0435\
  \u0437 \u0435\u0433\u043E \u0443\u0440\u043E\u043D\u0438\u0442\u044C.\u2026"
title: "\u041E\u0431\u0440\u0430\u0431\u043E\u0442\u043A\u0430 \u043E\u0448\u0438\u0431\
  \u043E\u043A"
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
