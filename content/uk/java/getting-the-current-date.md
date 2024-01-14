---
title:                "Java: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Чому

Уявіть ситуацію, в якій ви хочете знати, який зараз день тижня або ж яка дата на календарі. Це може бути корисно при створенні програми з нагадуваннями або просто для власного контролю. Отримання поточної дати - це універсальна задача в багатьох програмах, тому це корисно знати як зробити це в Java.

# Як

Простий спосіб отримати поточну дату в Java - використовувати клас `java.util.Date` та метод `now()`:

```Java
import java.util.Date;

public class CurrentDateExample {

    public static void main(String[] args) {
        Date now = new Date();
        System.out.println(now); // Виведе поточну дату та час
    }

}
```

Ви також можете відформатувати вихідний рядок за допомогою класу `java.text.SimpleDateFormat`:

```Java
import java.util.Date;
import java.text.SimpleDateFormat;

public class CurrentDateExample {

    public static void main(String[] args) {
        Date now = new Date();
        SimpleDateFormat format = new SimpleDateFormat("dd.MM.yyyy");
        String formattedDate = format.format(now);
        System.out.println(formattedDate); // Виведе будь-який формат дати, наприклад "23.08.2021"
    }

}
```

# Глибокий погляд

Отримання поточної дати в Java може бути більш складною задачею, ніж здається на перший погляд. Це пов'язано з тим, що у Java є декілька різних типів дати, кожен з яких має свої особливості та обмеження.

Наприклад, клас `java.util.Date` базується на часовій зоні та зберігає час в мілісекундах з певної дати в минулому. Цей клас має обмеження на діапазон років, тому його використання може призвести до помилки в окремих ситуаціях.

Тому рекомендується використовувати клас `java.time.LocalDate` для роботи з датами. Він зберігає дату без часу та часової зони, що робить його більш універсальним та менш схильним до помилок.

# Дивись також

- [Офіційна документація Java - java.util.Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Офіційна документація Java - java.time.LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Стаття про різницю між класами Date та LocalDate](https://www.baeldung.com/java-date-vs-localdate)