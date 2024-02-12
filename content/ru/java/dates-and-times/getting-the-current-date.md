---
title:                "Получение текущей даты"
aliases:
- /ru/java/getting-the-current-date.md
date:                  2024-01-28T23:59:19.919139-07:00
model:                 gpt-4-0125-preview
simple_title:         "Получение текущей даты"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?

Получение текущей даты в Java - это проще простого – дело в использовании правильного класса, чтобы захватить сегодняшний день из системных часов. Программисты делают это для слежения, логирования, или чтобы добавить в свои приложения функции, чувствительные ко времени.

## Как это сделать:

### Получаем сегодняшнюю дату

```java
import java.time.LocalDate;

public class Main {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        System.out.println("Сегодняшняя дата: " + today);
    }
}
```

**Пример вывода:**
```
Сегодняшняя дата: 2023-04-01
```

### Время с дополнительными деталями

```java
import java.time.LocalDateTime;

public class Main {
    public static void main(String[] args) {
        LocalDateTime now = LocalDateTime.now();
        System.out.println("Текущие дата и время: " + now);
    }
}
```

**Пример вывода:**
```
Текущие дата и время: 2023-04-01T12:45:30.123
```

## Подробнее:

До Java 8 за работу с датой и временем отвечали `java.util.Date` и `java.util.Calendar`. Но они были громоздкими и неинтуитивными. В Java 8 был представлен `java.time`, более надежный и понятный API. `java.time.LocalDate` получает дату без времени, в то время как `java.time.LocalDateTime` получает и дату, и время, без часового пояса. Если вам нужен часовой пояс, есть `java.time.ZonedDateTime`. А для получения только времени существует `java.time.LocalTime`.

Что касается альтернатив, до Java 8 существовала библиотека Joda-Time, и некоторые устаревшие проекты могут все еще использовать ее. Но с появлением пакета `java.time` в Java 8, он считается стандартом, и по уважительным причинам. Это всеобъемлюще и соответствует календарной системе ISO-8601.

С точки зрения реализации, методы `now()` в классах `java.time` получают текущую дату/время из системных часов, которые являются компьютерным представлением текущего времени, связанным с реальным миром через системные настройки и синхронизацию времени через интернет.

## Смотрите также:

- Официальная документация пакета `java.time`: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Стандарты даты и времени ISO 8601: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)
- Для управления датой в старом стиле Java, смотрите класс `Calendar`: [https://docs.oracle.com/javase/7/docs/api/java/util/Calendar.html](https://docs.oracle.com/javase/7/docs/api/java/util/Calendar.html)
