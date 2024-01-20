---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що й навіщо?
Отримання поточної дати в Java - це спосіб отримати інформацію про дату та час в момент виконання програми. Це важливо для логування, відстеження подій або виведення дати на екран.

## Як це зробити:
Щоб отримати поточну дату і час в Java, використовуйте клас `LocalDateTime` з пакету `java.time`.

```Java
import java.time.LocalDateTime;

public class Main {
    public static void main(String[] args) {
        LocalDateTime current = LocalDateTime.now();
        System.out.println("Поточна дата та час: " + current);
    }
}
```

Коли ви виконуєте цей код, вихід буде подібний до:

```Shell
Поточна дата та час: 2020-12-05T10:15:30.908873
```

## Занурення в деталі:
Перші версії Java надавали доступ до поточної дати через клас `Date`. Однак цей клас був важким для розуміння і використання, тому в Java 8 було додано новий API часу, зокрема клас `LocalDateTime`.

Ви також можете використовувати `ZonedDateTime` або `Instant` для отримання дати і часу з урахуванням часового поясу.

Наявність кількох варіантів може бути збентежливою, але вибір між ними залежить від вимог до вашої програми. Виберіть той, який найкраще підходить для ваших потреб.

## На сторінках інших джерел:
1. Oracle Java Documentation: [LocalDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
2. Oracle Java Documentation: [ZonedDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/ZonedDateTime.html)
3. Oracle Java Documentation: [Instant](https://docs.oracle.com/javase/8/docs/api/java/time/Instant.html)
4. [Java - Getting Current Date and Time](https://www.javatpoint.com/java-get-current-date)