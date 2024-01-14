---
title:                "Java: Отримання поточної дати"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Почему

Каждый день мы используем текущую дату в различных приложениях и системах. Получить актуальную дату является необходимостью для правильного отображения информации и выполнения различных операций. В Java, получение текущей даты является важной задачей, которая может быть выполнена с помощью нескольких простых шагов.

## Как

Для получения текущей даты в Java, мы можем использовать класс `LocalDate` из пакета `java.time`. Вот пример кода, который показывает, как получить текущую дату и вывести ее в консоль:

```Java
import java.time.LocalDate;

public class DateExample {

    public static void main(String[] args) {

        // получаем текущую дату
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // выводим дату в консоль

    }
}
```

В данном примере мы получаем текущую дату с помощью метода `now()` класса `LocalDate` и выводим ее в консоль с помощью метода `println()`. При запуске программы мы получим следующий вывод:

```
2021-08-20
```

Кроме того, мы можем изменить формат отображения даты с помощью метода `format()` и объекта `DateTimeFormatter`. Например, таким образом мы можем получить дату в формате `dd/MM/yyyy`:

```Java
// получаем текущую дату
LocalDate currentDate = LocalDate.now();

// определяем формат отображения даты
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");

// применяем формат и выводим дату в консоль
System.out.println(currentDate.format(formatter));
```

Вывод будет следующим:

```
20/08/2021
```

## Глубокий погружение

Как мы уже упоминали, для работы с датами в Java мы используем класс `LocalDate` из пакета `java.time`. Этот класс представляет собой неизменяемую дату без временной зоны, то есть он содержит только информацию о дне, месяце и годе. Класс `LocalDate` имеет множество методов для работы с датами, например, мы можем получить конкретную дату, добавить или вычесть дни, месяцы или годы и многое другое.

Также в Java есть классы `LocalDateTime` и `ZonedDateTime`, которые представляют дату и время, а также учитывают временную зону. Для получения текущего времени в Java мы можем использовать класс `LocalTime` или его расширенный вариант `LocalDateTime`. Эти классы также имеют различные методы для работы с датами и временем.

В Java 8 был добавлен новый API для работы с датами и временем - `java.time`. Он предлагает удобный и надежный способ работы с датами в приложениях и имеет более удобный синтаксис по сравнению с устаревшим классом `Date`.

## Дивіться також

- [Официальная документация Java: LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Примеры использования класса LocalDate](https://www.baeldung.com/java-localdate)
- [Java 8 Date Time API Tutorial](https://www.journaldev.com/2800/java-8-date-localdate-localdatetime-instant)