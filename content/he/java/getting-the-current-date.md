---
title:                "קבלת התאריך הנוכחי"
date:                  2024-01-20T15:15:29.940671-07:00
html_title:           "C: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?
לקבל את התאריך הנוכחי ב-Java היא תכנית בסיסית שכל מפתח צריך לדעת. אנחנו עושים את זה ללוגים, תזמון עבודות, ובדיקות זמן-אמת.

## איך לעשות:
```java
import java.time.LocalDate;

public class GetCurrentDate {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println("Today's Date: " + currentDate);
    }
}
```
תוצאת ספל:
```
Today's Date: 2023-04-05
```

## עיון מעמיק
לקבלת התאריך הנוכחי, Java השתמשה במחלקות שונות לאורך השנים. בתחילה היה `java.util.Date`, אבל הוא לא היה זמין לזמן אזורי. לאחר מכן בא `java.util.Calendar`, שהוסיף פונקציונליות אבל היה כבד ומבלבל. מ-Java 8 והלאה, אנו משתמשים ב-`java.time`, חבילה מודרנית וחזקה לניהול זמן ותאריך.

דרך נוספת לקבל את התאריך היא להשתמש ב-`java.time.LocalDateTime` או `java.time.ZonedDateTime` אם אתם רוצים לכלול מידע על השעה או את אזור הזמן שלכם.

`LocalDate.now()` היא פונקציה חזקה שמחזירה את התאריך הנוכחי לפי שעון המערכת ואזור הזמן של ה-JVM (Java Virtual Machine), והיא מציעה שפע של אפשרויות לניפוי והשוואה.

## ראה גם:

- [Java 8 Date/Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Oracle Java Tutorials – Date Time](https://docs.oracle.com/javase/tutorial/datetime/)
- [Baeldung – Guide to LocalDate](https://www.baeldung.com/java-8-date-time-intro)