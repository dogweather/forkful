---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:57.705682-07:00
description: "\u0421\u0440\u0430\u0432\u043D\u0435\u043D\u0438\u0435 \u0434\u0432\u0443\
  \u0445 \u0434\u0430\u0442 \u0437\u0430\u043A\u043B\u044E\u0447\u0430\u0435\u0442\
  \u0441\u044F \u0432 \u043E\u043F\u0440\u0435\u0434\u0435\u043B\u0435\u043D\u0438\
  \u0438 \u0442\u043E\u0433\u043E, \u043F\u0440\u0435\u0434\u0448\u0435\u0441\u0442\
  \u0432\u0443\u0435\u0442 \u043B\u0438 \u043E\u0434\u043D\u0430 \u0434\u0430\u0442\
  \u0430 \u0434\u0440\u0443\u0433\u043E\u0439, \u0441\u043B\u0435\u0434\u0443\u0435\
  \u0442 \u0437\u0430 \u043D\u0435\u0439 \u0438\u043B\u0438 \u0441\u043E\u0432\u043F\
  \u0430\u0434\u0430\u0435\u0442 \u0441 \u043D\u0435\u0439. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\
  \ \u044D\u0442\u043E \u0434\u043B\u044F\u2026"
lastmod: '2024-03-13T22:44:44.845319-06:00'
model: gpt-4-0125-preview
summary: "\u0421\u0440\u0430\u0432\u043D\u0435\u043D\u0438\u0435 \u0434\u0432\u0443\
  \u0445 \u0434\u0430\u0442 \u0437\u0430\u043A\u043B\u044E\u0447\u0430\u0435\u0442\
  \u0441\u044F \u0432 \u043E\u043F\u0440\u0435\u0434\u0435\u043B\u0435\u043D\u0438\
  \u0438 \u0442\u043E\u0433\u043E, \u043F\u0440\u0435\u0434\u0448\u0435\u0441\u0442\
  \u0432\u0443\u0435\u0442 \u043B\u0438 \u043E\u0434\u043D\u0430 \u0434\u0430\u0442\
  \u0430 \u0434\u0440\u0443\u0433\u043E\u0439, \u0441\u043B\u0435\u0434\u0443\u0435\
  \u0442 \u0437\u0430 \u043D\u0435\u0439 \u0438\u043B\u0438 \u0441\u043E\u0432\u043F\
  \u0430\u0434\u0430\u0435\u0442 \u0441 \u043D\u0435\u0439. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B \u0434\u0435\u043B\u0430\u044E\u0442\
  \ \u044D\u0442\u043E \u0434\u043B\u044F\u2026"
title: "\u0421\u0440\u0430\u0432\u043D\u0435\u043D\u0438\u0435 \u0434\u0432\u0443\u0445\
  \ \u0434\u0430\u0442"
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
