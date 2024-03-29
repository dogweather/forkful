---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:34.059066-07:00
description: "\u041F\u0435\u0447\u0430\u0442\u044C \u043E\u0442\u043B\u0430\u0434\u043E\
  \u0447\u043D\u043E\u0433\u043E \u0432\u044B\u0432\u043E\u0434\u0430 \u2014 \u044D\
  \u0442\u043E, \u043F\u043E \u0441\u0443\u0442\u0438, \u0441\u043F\u043E\u0441\u043E\
  \u0431 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u0430\
  \ \u043F\u043E\u0434\u0433\u043B\u044F\u0434\u0435\u0442\u044C, \u0447\u0442\u043E\
  \ \u043F\u0440\u043E\u0438\u0441\u0445\u043E\u0434\u0438\u0442 \u0432\u043D\u0443\
  \u0442\u0440\u0438 \u0435\u0433\u043E \u043A\u043E\u0434\u0430 \u0432 \u0440\u0435\
  \u0430\u043B\u044C\u043D\u043E\u043C \u0432\u0440\u0435\u043C\u0435\u043D\u0438\
  . \u042D\u0442\u043E \u043A\u0440\u0438\u0442\u0438\u0447\u0435\u0441\u043A\u0438\
  \ \u0432\u0430\u0436\u043D\u043E \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:44.983425-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0435\u0447\u0430\u0442\u044C \u043E\u0442\u043B\u0430\u0434\u043E\
  \u0447\u043D\u043E\u0433\u043E \u0432\u044B\u0432\u043E\u0434\u0430 \u2014 \u044D\
  \u0442\u043E, \u043F\u043E \u0441\u0443\u0442\u0438, \u0441\u043F\u043E\u0441\u043E\
  \u0431 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0438\u0441\u0442\u0430\
  \ \u043F\u043E\u0434\u0433\u043B\u044F\u0434\u0435\u0442\u044C, \u0447\u0442\u043E\
  \ \u043F\u0440\u043E\u0438\u0441\u0445\u043E\u0434\u0438\u0442 \u0432\u043D\u0443\
  \u0442\u0440\u0438 \u0435\u0433\u043E \u043A\u043E\u0434\u0430 \u0432 \u0440\u0435\
  \u0430\u043B\u044C\u043D\u043E\u043C \u0432\u0440\u0435\u043C\u0435\u043D\u0438\
  . \u042D\u0442\u043E \u043A\u0440\u0438\u0442\u0438\u0447\u0435\u0441\u043A\u0438\
  \ \u0432\u0430\u0436\u043D\u043E \u0434\u043B\u044F\u2026"
title: "\u0412\u044B\u0432\u043E\u0434 \u043E\u0442\u043B\u0430\u0434\u043E\u0447\u043D\
  \u043E\u0439 \u0438\u043D\u0444\u043E\u0440\u043C\u0430\u0446\u0438\u0438"
---

{{< edit_this_page >}}

## Что и Почему?
Печать отладочного вывода — это, по сути, способ программиста подглядеть, что происходит внутри его кода в реальном времени. Это критически важно для отслеживания ошибок и понимания потока выполнения кода без установки формальных инструментов отладки или сессий.

## Как сделать:
Давайте печатать что-нибудь в консоль:

```Kotlin
fun main() {
    val magicNumber = 42
    println("Волшебное число: $magicNumber")

    debugPrint("Волшебное число в квадрате равно ${magicNumber * magicNumber}")
}

fun debugPrint(message: String) {
    if (BuildConfig.DEBUG) {
        println("DEBUG: $message")
    }
}
```
Пример вывода:
```
Волшебное число: 42
DEBUG: Волшебное число в квадрате равно 1764
```
Быстро и непринужденно, вы видите свои значения прямо там, в консоли.

## Глубже
Печать в консоль для отладки стара как мир. Это просто, это распространено во всех языках программирования, и это справляется со своей задачей. Но это не изысканно, и в сложных системах слишком много вывода может создать беспорядок.

Альтернативы `println` в Kotlin могут включать использование фреймворков для логирования, таких как `Log4j`, или встроенного в Kotlin инструмента `Logging`, который помогает фильтровать сообщения по уровням серьезности.

Одна из особенностей Kotlin, как видно на нашей функции `debugPrint`, заключается в проверке, находимся ли мы в сборке для отладки; Таким образом, мы не захламляем производственные журналы нашими отладочными сообщениями, сохраняя наши фактические развертывания чистыми и удобными для пользователя.

## Смотрите также
- Для введения в логирование в Kotlin, смотрите официальную документацию: [Kotlin Logging](https://github.com/MicroUtils/kotlin-logging)
- Взгляд JetBrains на стратегии отладки: [Отладка в IntelliJ IDEA](https://www.jetbrains.com/help/idea/debugging-code.html)
- Если вы используете Android, официальное руководство по использованию Logcat бесценно: [Документация по Android Logcat](https://developer.android.com/studio/command-line/logcat)
