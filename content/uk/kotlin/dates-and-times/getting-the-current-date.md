---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:34.854041-07:00
description: "\u042F\u043A: Kotlin \u043D\u0435 \u043C\u0430\u0454 \u0432\u043B\u0430\
  \u0441\u043D\u043E\u0433\u043E API \u0434\u043B\u044F \u0434\u0430\u0442\u0438 \u0442\
  \u0430 \u0447\u0430\u0441\u0443, \u0430\u043B\u0435 \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0454 \u0434\u043B\u044F \u0446\u0456\u0454\
  \u0457 \u0444\u0443\u043D\u043A\u0446\u0456\u043E\u043D\u0430\u043B\u044C\u043D\u043E\
  \u0441\u0442\u0456 Java Standard Library. \u041E\u0441\u044C \u044F\u043A \u043C\
  \u043E\u0436\u043D\u0430 \u043E\u0442\u0440\u0438\u043C\u0430\u0442\u0438 \u043F\
  \u043E\u0442\u043E\u0447\u043D\u0443 \u0434\u0430\u0442\u0443."
lastmod: '2024-03-13T22:44:49.236948-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u043D\u0435 \u043C\u0430\u0454 \u0432\u043B\u0430\u0441\u043D\u043E\
  \u0433\u043E API \u0434\u043B\u044F \u0434\u0430\u0442\u0438 \u0442\u0430 \u0447\
  \u0430\u0441\u0443, \u0430\u043B\u0435 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u0454 \u0434\u043B\u044F \u0446\u0456\u0454\u0457 \u0444\
  \u0443\u043D\u043A\u0446\u0456\u043E\u043D\u0430\u043B\u044C\u043D\u043E\u0441\u0442\
  \u0456 Java Standard Library."
title: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\
  \u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438"
weight: 29
---

## Як:


### Використовуючи стандартний Kotlin
Kotlin не має власного API для дати та часу, але використовує для цієї функціональності Java Standard Library. Ось як можна отримати поточну дату:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Сьогоднішня дата: $today")
}
```

**Приклад виводу:**
```
Сьогоднішня дата: 2023-04-05
```

### Використовуючи java.util.Date
Для операцій, які вимагають і дату, і час, ви можете віддати перевагу `java.util.Date`.

```kotlin
import java.util.Date

fun main() {
    val currentDate = Date()
    println("Поточна дата та час: $currentDate")
}
```

**Приклад виводу:**
```
Поточна дата та час: Срд Апр 05 15:20:45 GMT 2023
```

### Використовуючи бібліотеку Joda-Time
До того, як Java 8 ввела новий API для дати та часу, Joda-Time була де-факто стандартом для операцій з датою-часом у Java та Kotlin. Хоча тепер вона не є необхідною для багатьох проектів, деякі можуть все ще використовувати її з причин спадковості або особистих уподобань.

Додайте бібліотеку Joda-Time до файлу build.gradle вашого проекту:
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Сьогоднішня дата: $today")
}
```

**Приклад виводу:**
```
Сьогоднішня дата: 2023-04-05
```

### Використовуючи ThreeTenABP для Android
Для розробки під Android рекомендується використовувати бекпорт Java Time API за допомогою проекту ThreeTen Android Backport для версій до Android API Level 26.

Додайте залежність до файлу build.gradle вашого додатка:
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

Ініціалізуйте його у вашому класі Application:
```kotlin
import android.app.Application
import com.jakewharton.threetenabp.AndroidThreeTen

class MyApp : Application() {
    override fun onCreate() {
        super.onCreate()
        AndroidThreeTen.init(this)
    }
}
```

Потім ви можете використовувати його ось так:
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Сьогоднішня дата: $today")
}
```

**Приклад виводу:**
```
Сьогоднішня дата: 2023-04-05
```
