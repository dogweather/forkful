---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Що і навіщо?

1) Парсинг дати з рядка - це процес перетворення текстового представлення дати і часу на об'єкт `java.util.Date` або `java.time.LocalDate`. 
2) Програмісти це роблять, щоб маніпулювати цими даними під час розробки ПЗ.

## Як це зробити:

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Main {
    public static void main (String [] args) {
        String str = "2022-01-01";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(str, formatter);
        System.out.println(date);
    }
}
```

Виведення:

```text
2022-01-01
```

## Глибше занурення:

1) Історичний контекст: Перші спроби парсингу дати з рядка в програмуванні були величезною проблемою через різницю в загальноприйнятих форматах дати і часу. Java просто розв'язала цю проблему шляхом введення нового API для дати і часу в Java 8. 

2) Альтернативи: Сама Java надає багато способів парсингу дати з рядка. Ви також можете використовувати сторонні бібліотеки, наприклад Joda-Time.

3) Подробиці реалізації: За замовчуванням, класи Java для парсингу дати використовують шаблон ISO-8601. Ми можемо кастомізувати це, використовуючи `DateTimeFormatter`.

## Дивіться також:

1) [Java - Date Time] (https://www.tutorialspoint.com/java/java_date_time.htm)

2) [Java LocalDate] (https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)

3) [Java - DateTimeFormatter] (https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)

4) [Joda-Time Library] (https://www.joda.org/joda-time/)