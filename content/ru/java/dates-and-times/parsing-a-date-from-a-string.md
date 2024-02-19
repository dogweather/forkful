---
aliases:
- /ru/java/parsing-a-date-from-a-string/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:12.058015-07:00
description: "\u0420\u0430\u0437\u0431\u043E\u0440 \u0434\u0430\u0442\u044B \u0438\
  \u0437 \u0441\u0442\u0440\u043E\u043A\u0438 \u043E\u0437\u043D\u0430\u0447\u0430\
  \u0435\u0442 \u043F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\
  \u0438\u0435 \u0442\u0435\u043A\u0441\u0442\u0430 \u0432 \u043E\u0431\u044A\u0435\
  \u043A\u0442 Date, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u043C\u043E\u0436\
  \u0435\u0442 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0430. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\
  \ \u044D\u0442\u043E, \u0447\u0442\u043E\u0431\u044B \u043F\u043E\u043D\u0438\u043C\
  \u0430\u0442\u044C\u2026"
lastmod: 2024-02-18 23:08:56.853682
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0437\u0431\u043E\u0440 \u0434\u0430\u0442\u044B \u0438\u0437\
  \ \u0441\u0442\u0440\u043E\u043A\u0438 \u043E\u0437\u043D\u0430\u0447\u0430\u0435\
  \u0442 \u043F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0442\u0435\u043A\u0441\u0442\u0430 \u0432 \u043E\u0431\u044A\u0435\u043A\
  \u0442 Date, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u043C\u043E\u0436\u0435\
  \u0442 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\
  \ \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u043C\u0430. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\
  \ \u044D\u0442\u043E, \u0447\u0442\u043E\u0431\u044B \u043F\u043E\u043D\u0438\u043C\
  \u0430\u0442\u044C\u2026"
title: "\u0410\u043D\u0430\u043B\u0438\u0437 \u0434\u0430\u0442\u044B \u0438\u0437\
  \ \u0441\u0442\u0440\u043E\u043A\u0438"
---

{{< edit_this_page >}}

## Что и зачем?

Разбор даты из строки означает преобразование текста в объект Date, который может использовать программа. Программисты делают это, чтобы понимать пользовательский ввод или данные, хранящиеся в удобочитаемых форматах.

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
