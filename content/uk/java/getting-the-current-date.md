---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:15:31.978937-07:00
simple_title:         "Отримання поточної дати"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Отримання поточної дати в Java – це як глянути на годинник. Ми робимо це, щоб маркувати час подій, логувати дії, міряти проміжки часу та багато іншого.

## How to: (Як зробити:)
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class GetCurrentDate {
    public static void main(String[] args) {
        // Стандартний спосіб отримання поточної дати
        LocalDate currentDate = LocalDate.now();
        System.out.println("Current Date: " + currentDate);

        // Форматування дати
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy");
        String formattedDate = currentDate.format(formatter);
        System.out.println("Formatted Date: " + formattedDate);
    }
}
```
Sample Output:
```
Current Date: 2023-03-15
Formatted Date: 15-03-2023
```

## Deep Dive (Поглиблений огляд):
Once upon a time in Java, we were limited to `java.util.Date`, but it was troublesome, particularly with timezone quirks. Java 8 introduced `java.time`, the more robust and intuitive date and time API. 

Alternatives? Of course, there's `Calendar`, but it's a bit clunky. Libraries like Joda-Time were the go-to before Java 8 but are now somewhat obsolete. 

Internals? `LocalDate.now()` grabs the system clock from the default time-zone. Under the hood, it ultimately calls `Clock.systemDefaultZone().instant()` to get the current instant in time, which it converts to a date. 

Note that `LocalDate` doesn't contain time or timezone data. If you need those, look into `LocalDateTime` or `ZonedDateTime`.

## See Also (Дивіться також):
- Official Oracle tutorials: [Date Time](https://docs.oracle.com/javase/tutorial/datetime/)
- JavaDoc for `LocalDate`: [LocalDate](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)
- Want timezone management? [ZonedDateTime](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/ZonedDateTime.html)
- Compare with old: [Date](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html) vs [Calendar](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Calendar.html)
