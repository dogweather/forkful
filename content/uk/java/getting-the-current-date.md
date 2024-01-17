---
title:                "Отримання поточної дати."
html_title:           "Java: Отримання поточної дати."
simple_title:         "Отримання поточної дати."
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що і для чого?
Отримання поточної дати - це процес отримання поточної дати та часу на вашому комп'ютері. Це дозволяє програмістам організувати свої програми відповідно до поточної дати та часу, що є важливим для функцій, які виконуються в певний час або для ведення журналів.

## Як це зробити:
```java
// Імпортуємо необхідний пакет
import java.time.LocalDateTime;

// Використовуємо метод now() для отримання поточної дати та часу
LocalDateTime now = LocalDateTime.now();

// Виводимо поточну дату та час
System.out.println(now);
```
**Вихідний результат:** `2019-07-18T14:08:34.674`

## Глибоке дослідження:
* Історичний контекст: у попередні версії Java, для отримання поточної дати використовувався клас `Date`, але він був має вади, які були виправлені в пакеті `java.time` у Java 8.
* Альтернативи: можна також отримати поточну дату та час з використанням сторонніх бібліотек, наприклад, `Joda-Time`.
* Деталі реалізації: Java використовує так званий "нульовий мередіан", який представляє собою кількість мілісекунд, що минули з 00:00:00 UTC 1 січня 1970 року, для отримання поточної дати та часу.

## Дивись також:
* [Офіційна документація Java - LocalDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
* [Стаття про отримання поточної дати в Java](https://www.geeksforgeeks.org/java-program-to-display-current-date-time-in-12-hour-format/)
* [Joda-Time бібліотека](https://www.joda.org/joda-time/)