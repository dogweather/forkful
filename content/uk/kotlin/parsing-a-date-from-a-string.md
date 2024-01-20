---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Парсинг дати з рядка - це процес перетворення специфічної текстової інформації про дату в об'єкт дати. Програмісти це роблять, щоб мати можливість працювати з датами в коді більш зручним і ефективним чином.

## Як зробити:
Використовуємо клас SimpleDateFormat з Java для перетворення рядка в Date:

``` Kotlin
import java.text.SimpleDateFormat
import java.util.Date

fun main() {
    val dateString = "2022-01-01"
    val format = SimpleDateFormat("yyyy-MM-dd")
    val date: Date = format.parse(dateString)
    println(date)
}
```

Як результат, ми отримуємо:

``` Kotlin
Sat Jan 01 00:00:00 EET 2022
```

## Поглиблений огляд:
1. **Історичний контекст**: Парсинг дати з рядка є давнім викликом в програмуванні. Родоначальником методу слугує Java та її клас `SimpleDateFormat`, який донині активно використовують.
2. **Альтернативи**: ми можемо використовувати LocalDate parse() з Java 8, яка приймає рядок і виводить `LocalDate` замість `Date`.
3. **Деталі реалізації**: `SimpleDateFormat` вимагає від нас форматування рядка, який ми передаємо. Якщо форматування некоректне, генерується виняток ParseException. Тому є важливо мати сформований коректний рядок для перетворення.

## Дивись також:
1. [Java SimpleDateFormat](https://developer.android.com/reference/java/text/SimpleDateFormat)
2. [Java LocalDate](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)