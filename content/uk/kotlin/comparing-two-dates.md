---
title:                "Порівняння двох дат"
html_title:           "Kotlin: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що і чому?
Порівняння двох дат є важливою задачею для програмістів, оскільки це дозволяє визначити, яка з дат є більшою або меншою. Це допомагає при сортуванні і порівнянні даних, а також при встановленні часу подій в програмі.

## Як це зробити:
```
val date1 = LocalDate.of(2021, 10, 1)
val date2 = LocalDate.of(2021, 10, 5)

println("Перша дата більша за другу: ${date1.isAfter(date2)}")   // виведе "false"
println("Перша дата менша за другу: ${date1.isBefore(date2)}")  // виведе "true"
```

## Глибокий погляд:
Порівняння дат є важливим елементом у більш старих мов програмування, таких як Java та С++. У Kotlin, це було спрощено за допомогою вбудованих методів ```isBefore()``` та ```isAfter()```. Окрім порівняння дат, також можна використовувати і інші методи, наприклад, визначення різниці між двома датами.

## Дивись також:
- [Kotlin документація про порівняння дат](https://kotlinlang.org/docs/datetime.html#comparing-dates)
- [Java документація про порівняння дат](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html#isBefore-java.time.chrono.ChronoLocalDate-)