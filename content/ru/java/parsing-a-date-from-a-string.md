---
title:                "Анализ даты из строки"
date:                  2024-01-29T00:00:12.058015-07:00
model:                 gpt-4-0125-preview
simple_title:         "Анализ даты из строки"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
