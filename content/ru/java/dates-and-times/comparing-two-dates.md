---
title:                "Сравнение двух дат"
aliases:
- /ru/java/comparing-two-dates.md
date:                  2024-01-28T23:55:57.705682-07:00
model:                 gpt-4-0125-preview
simple_title:         "Сравнение двух дат"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/java/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Сравнение двух дат заключается в определении того, предшествует ли одна дата другой, следует за ней или совпадает с ней. Программисты делают это для управления расписаниями, сроками, хронологической сортировкой и многим другим.

## Как:
Java значительно упрощает сравнение дат. Используйте `LocalDate` и методы `compareTo`, `isBefore` или `isAfter`. Вот краткое руководство:

```java
import java.time.LocalDate;

public class DateComparison {
    public static void main(String[] args) {
        LocalDate date1 = LocalDate.of(2023, 4, 1);
        LocalDate date2 = LocalDate.now(); // предположим, что сегодня 2023-4-15

        // Использование compareTo
        int comparisonResult = date1.compareTo(date2);
        if(comparisonResult < 0) {
            System.out.println("Date1 предшествует Date2");
        } else if (comparisonResult > 0) {
            System.out.println("Date1 следует за Date2");
        } else {
            System.out.println("Date1 совпадает c Date2");
        }

        // Использование isBefore и isAfter
        if(date1.isBefore(date2)) {
            System.out.println("Date1 раньше, чем Date2");
        } else if(date1.isAfter(date2)) {
            System.out.println("Date1 позже, чем Date2");
        } else {
            System.out.println("Date1 тот же самый день, что и Date2");
        }
    }
}
```

Пример вывода для сегодняшней даты 2023-04-15:

```
Date1 предшествует Date2
Date1 раньше, чем Date2
```

## Подробнее
Исторически обработка дат в Java была, ну, головной болью. Но затем появилась Java 8 с `java.time`, что стало переломным моментом. Теперь мы используем `LocalDate` для дат без времени. Хотите сравнивать даты, включая время? Обратите внимание на `LocalDateTime`.

Альтернативы? Конечно. До Java 8 были `java.util.Date` и `java.util.Calendar`. Вы все еще можете их использовать, но зачем себе усложнять жизнь?

С точки зрения реализации, `compareTo` возвращает `int`: отрицательное значение, если вызывающий объект меньше (ранее), ноль, если равен, положительное значение, если больше (позже). `isBefore` и `isAfter` возвращают `boolean`. Легко понять и без подводных камней.

## См. также
Для получения дополнительной информации изучите следующие ресурсы:

- [Документация Oracle по LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Учебное пособие Oracle по дате и времени](https://docs.oracle.com/javase/tutorial/datetime/)
- Stack Overflow для практического использования и устранения неполадок:
  - [Использование `LocalDate`](https://stackoverflow.com/questions/tagged/localdate)
  - [Java Date против Calendar](https://stackoverflow.com/questions/5369682/get-current-time-and-date-on-android)
