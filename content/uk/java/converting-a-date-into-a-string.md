---
title:                "Java: Перетворення дати в рядок."
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Чому

Перетворення дати в рядок є важливим елементом в програмуванні, оскільки дата може бути введена або збережена у вигляді рядка, а також використовується для збереження інформації у базах даних. Також цей процес може зробити роботу з датами більш зручною для користувачів.

##Як зробити

Існує кілька способів перетворити дату в рядок у мові програмування Java. Перший спосіб - використання класу SimpleDateFormat, який дозволяє визначити формат рядка дати. Наприклад:

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
Date date = new Date();
String dateString = dateFormat.format(date);

System.out.println(dateString);
```

Вивід: 05/12/2021

Інший спосіб - використання класу DateTimeFormatter, який був доданий у Java 8. Він дозволяє більш гнучко визначити формат дати, а також працює швидше за SimpleDateFormat. Приклад коду:

```Java
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
LocalDateTime dateTime = LocalDateTime.now();
String dateString = dateTime.format(formatter);

System.out.println(dateString);
```

Вивід: 05/12/2021

##Глибше вдивимося

Іноді може бути потрібно перетворити дату в рядок з конкретною місцевою часовою зоною або форматом відображення. Також важливо пам'ятати, що введення дати користувачем може бути невірним або не відповідати заданому формату, тому необхідно робити перевірку даних.

##Дивись також

- [Документація по класу SimpleDateFormat](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Документація по класу DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Стаття про роботу з датами в Java](https://www.baeldung.com/java-dates)