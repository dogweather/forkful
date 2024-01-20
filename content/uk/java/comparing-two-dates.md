---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що і чому?

Порівняння двох дат – це процес визначення різниці, рівності або порядку поміж двох часових відміток. Програмісти цим займаються, щоб розв'язувати проблеми, пов'язані з часом - все від очікування подій до обмеження терміну дії.

## Як це зробити:

Ми будемо використовувати клас `LocalDate`, що входить до пакету `java.time`(з Java 8 і вище). Спершу, створимо два об'єкти `LocalDate`.

```Java
// Import the LocalDate class
import java.time.LocalDate;

// Create two dates
LocalDate date1= LocalDate.of(2022, 5, 30);
LocalDate date2 = LocalDate.of(2021, 3, 12);
```

Тепер можемо порівнювати наші дати.

```Java
// Compare two dates
if(date1.isBefore(date2)){
    System.out.println(date1 + " is before " + date2);
}else if (date1.isAfter(date2)){
    System.out.println(date1 + " is after " + date2);
}else{
    System.out.println(date1 + " is equal to  " + date2);
}
```

Ви можете замінити дати у коді вище і виконати його, щоб подивитися на результат.

## Поглиблений огляд

1. Історичний контекст: В ранніх версіях Java, дати порівнювались за допомогою `java.util.Date` і `java.util.Calendar`. Проте, з Java 8, клас `LocalDate` забезпечує чіткіше і безпечніше API.
2. Альтернативи: Існує кілька бібліотек, як-от Joda-Time, що пропонують більш широкий функціонал. Але, в більшості випадків, вбудовані можливості Java досить достатні.
3. Деталі впровадження: `LocalDate` представляє дату на основі календаря ISO 8601 без часу і зони.

## Дивіться також

[Документація Oracle по LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)

[Joda-Time - бібліотека для часу і дати для Java](https://www.joda.org/joda-time/) 

[A Comparison of Java Date/Time Libraries](https://www.baeldung.com/java-date-time-libs)