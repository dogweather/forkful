---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:15:58.506632-07:00
simple_title:         "Отримання поточної дати"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Отримання поточної дати – це процес, в якому ваша програма дізнається, який зараз день. Це важливо для логування, тайм-стампів, функціоналу, що залежить від дати.

## How to: (Як це зробити:)
```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Сьогодні: $today")
}
```
Вивід:
```
Сьогодні: 2023-04-01
```

## Deep Dive (Занурення у деталі)
Ще з часів JDK 1.0, отримання поточної дати в Java було можливо, а з появою Kotlin він успадкував бібліотеки Java, включно `java.util.Date`. Але `java.time` - новіша і вдосконалена бібліотека з Java 8, яка автоматично враховує часові зони і більш безпечна для потоків. В Kotlin це теж справедливо.

Альтернатива `LocalDate.now()` – це використання `Calendar.getInstance()`, але `java.time` краще, тому що є не змінюваним(immutable) і більш інтуитивно зрозумілим.

Невеличкий челендж: спробуйте змінити формат виводу дати або отримання поточного часу окремо!

## See Also (Додаткові ресурси)
- [Документація Java LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Best practices for working with dates and times](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html) (Кращі практики роботи з датами та часом)
- [Kotlin documentation on handling dates](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.js/-date/) (Документація Kotlin по роботі з датами)
