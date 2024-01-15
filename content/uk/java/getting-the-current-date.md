---
title:                "Отримання поточної дати"
html_title:           "Java: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Зачем

Ответ на этот вопрос кажется очевидным - чтобы узнать текущую дату. Но зачем это нужно в программировании? Очень часто нам нужно работать с различными временными интервалами и датами, а получение текущей даты позволяет нам упростить этот процесс.

## Как

```Java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        // Получаем текущую дату
        LocalDate currentDate = LocalDate.now();
        // Выводим ее на экран в формате "год-месяц-день"
        System.out.println("Текущая дата: " + currentDate);
        // Выводим только год
        System.out.println("Текущий год: " + currentDate.getYear());
        // Выводим только месяц
        System.out.println("Текущий месяц: " + currentDate.getMonth());
        // Выводим только день
        System.out.println("Текущий день: " + currentDate.getDayOfMonth());
    }
}
```

```
Текущая дата: 2021-01-01
Текущий год: 2021
Текущий месяц: JANUARY
Текущий день: 1
```

В приведенном примере мы использовали класс `LocalDate` из пакета `java.time`, который позволяет нам работать с датами и временными интервалами. Мы вызвали статический метод `now()`, который возвращает текущую дату. Затем мы использовали различные методы этого класса для получения необходимой нам информации о текущей дате. Обратите внимание, что месяц представлен в виде `enum` типа `Month`, поэтому он выводится в различных форматах.

## Глубокий погружение

Если мы хотим получить не только дату, но и время, мы можем воспользоваться классом `LocalDateTime`. Также мы можем работать с различными часовыми поясами, используя класс `ZonedDateTime`. И если нам нужно сравнить текущую дату с какой-то другой, мы можем использовать методы для сравнения дат и временных интервалов из классов `LocalDateTime` и `ZonedDateTime`. Более подробную информацию о работе с датами и временными интервалами в Java вы можете найти в [официальной документации](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html).

## Смотрите также

- [Официальная документация по работе с датами и временными интервалами в Java](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Статья о пакете `java.time` на сайте Oracle](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)