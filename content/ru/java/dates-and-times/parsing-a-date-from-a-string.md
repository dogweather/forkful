---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:12.058015-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Java \u0434\u043B\u044F \u044D\u0442\u043E\u0439 \u0437\u0430\
  \u0434\u0430\u0447\u0438 \u0441\u0443\u0449\u0435\u0441\u0442\u0432\u0443\u0435\u0442\
  \ \u043A\u043B\u0430\u0441\u0441 `java.time.format.DateTimeFormatter`. \u0412\u043E\
  \u0442 \u043A\u0430\u043A \u0441 \u043D\u0438\u043C \u0440\u0430\u0431\u043E\u0442\
  \u0430\u0442\u044C."
lastmod: '2024-03-13T22:44:44.839436-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Java \u0434\u043B\u044F \u044D\u0442\u043E\u0439 \u0437\u0430\u0434\
  \u0430\u0447\u0438 \u0441\u0443\u0449\u0435\u0441\u0442\u0432\u0443\u0435\u0442\
  \ \u043A\u043B\u0430\u0441\u0441 `java.time.format.DateTimeFormatter`."
title: "\u0410\u043D\u0430\u043B\u0438\u0437 \u0434\u0430\u0442\u044B \u0438\u0437\
  \ \u0441\u0442\u0440\u043E\u043A\u0438"
weight: 30
---

## Как это сделать:
В Java для этой задачи существует класс `java.time.format.DateTimeFormatter`. Вот как с ним работать.

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

public class DateParser {

    public static void main(String[] args) {
        String dateString = "2023-03-15";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        try {
            LocalDate date = LocalDate.parse(dateString, formatter);
            System.out.println("Разобранная дата: " + date);
        } catch (DateTimeParseException e) {
            System.err.println("Ой, дата была в неправильном формате!");
        }
    }
}
```

Запустите этот маленький скрипт, и вы увидите:

```
Разобранная дата: 2023-03-15
```

## Подробнее
Задолго до того, как `java.time` вошел в моду с Java 8 в 2014 году, люди мучились с `java.util.Date` и `SimpleDateFormat`. Эти старички не только враждебны к потокам, но и доставляют головную боль с их капризами часовых поясов.

В наши дни `java.time` - это новый крик моды. Он потокобезопасен, неизменяемый (без коварных изменений) и более понятный в намерениях. К тому же, вы можете выбрать из набора предопределенных форматеров или создать свой с использованием шаблонов.

Альтернативы, спрашиваете? Библиотеки, такие как Joda-Time, прокладывали путь, но поскольку java.time многому научился у их идей, большинство отказалось от них в пользу стандартной библиотеки.

Внутри, разбор с помощью `DateTimeFormatter` выполняет тяжелую работу. Он проверяет шаблоны, валидирует ввод, обрабатывает исключения и доставляет блестящий `LocalDate`, `LocalTime` или даже `ZonedDateTime` в зависимости от того, что вы ищете.

## Смотрите также
- Официальная документация Java для `java.time.format.DateTimeFormatter`: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
- Учебные пособия Oracle по Java, включая дату и время: https://docs.oracle.com/javase/tutorial/datetime/
