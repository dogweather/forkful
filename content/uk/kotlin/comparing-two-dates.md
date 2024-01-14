---
title:                "Kotlin: Порівняння двох дат"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

У подорожі в програмування Java чимало тих тем, які були відомі кілька років назад, не потребували попереднього думання про те, як встановлювати набір знарядь. Одним з таких тем є порівняння двох дат. Чому це важливо? Продовжимо читати, щоб дізнатися більше.

## Як це зробити

Порівнювати дві дати можна за допомогою класу **LocalDate** з пакету **java.time**. Ось приклад коду:

```Kotlin
val date1 = LocalDate.parse("2020-01-01")
val date2 = LocalDate.parse("2020-01-05")

if (date1.isBefore(date2)) {
    println("$date1 is before $date2")
} else if (date1.isAfter(date2)) {
    println("$date1 is after $date2")
} else {
    println("$date1 is equal to $date2")
}
```

Вихідні дані будуть наступними:

```
2020-01-01 is before 2020-01-05
```

## Глибоке дослідження

Тепер, коли ми знаємо, як порівнювати дві дати, давайте розгледимо деякі нюанси цього процесу.

- Для порівняння дат потрібно використовувати методи **isBefore()** та **isAfter()**, а не оператори `>` або `<`. Це пов'язано з тим, що дати є об'єктами, а не примітивами.
- Метод **isEqual()** перевіряє рівність об'єктів дат, а не той факт, що вони знаходяться в одній точці часу.
- Дати можна порівнювати не тільки за допомогою методів **isBefore()** та **isAfter()**, але і за допомогою методу **compareTo()**, який повертає значення `0`, `1` або `-1`.
- Для зручності можна використовувати оператори `==` та `!=` для порівняння дат, але в цьому випадку будуть використовуватися методи **equals()** та **compareTo()** під капотом.

## Дивіться також

- [Офіційна документація з класом **LocalDate** (англ.)](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Порівняння дат в Java: як правильно й неправильно (укр.)](https://medium.com/@slyrutrait/java-equals-instant-before-and-after-f4031328b0b7)

## *Автор: Mariia Petrova*