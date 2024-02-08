---
title:                "Отримання поточної дати"
aliases:
- uk/kotlin/getting-the-current-date.md
date:                  2024-02-03T19:10:34.854041-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отримання поточної дати"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
У програмуванні отримання поточної дати є фундаментальним завданням, яке дозволяє розробникам доступатися до поточної дати, відображати її або маніпулювати нею в своїх додатках. Ця можливість є критично важливою для чого завгодно, починаючи з логування та маркування часом подій і закінчуючи розрахунками, заснованими на датах.

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
