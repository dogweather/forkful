---
title:                "פרסום תאריך ממחרוזת"
aliases:
- /he/java/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:39.771195-07:00
model:                 gpt-4-0125-preview
simple_title:         "פרסום תאריך ממחרוזת"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
פיענוח תאריך ממחרוזת כולל המרת הייצוג הטקסטואלי של תאריך ושעה לאובייקט מסוג `Date` או לאובייקט `LocalDateTime` מודרני יותר. מתכנתים עושים זאת כדי לנהל, לעצב, להשוות או לאחסן תאריכים בפורמט מתוקנן, מה שחשוב מאוד ליישומים הדורשים חישובים, אימות או אינטרנשיונליזציה עקבית של תאריכים.

## איך לעשות:

### באמצעות חבילת `java.time` (מומלץ בJava 8 ואילך):
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // פלט: 2023-04-30
    }
}
```

### באמצעות `SimpleDateFormat` (גישה ישנה יותר):
```java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "30/04/2023";
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        try {
            Date date = formatter.parse(dateString);
            System.out.println(date); // פורמט הפלט תלוי בפורמט הברירת המחדל של המערכת שלך
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### באמצעות ספריות צד שלישי (לדוגמה, Joda-Time):
Joda-Time היה ספריית צד שלישי משמעותית אך כעת הוא נמצא במצב תחזוקה בשל הכנסת חבילת ה-`java.time` ב-Java 8. עם זאת, לאלה שמשתמשים בגרסאות Java לפני 8, Joda-Time הוא בחירה טובה.
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // פלט: 2023-04-30
    }
}
```
שימו לב שכאשר עובדים עם תאריכים, תמיד יש להיות מודעים להגדרות אזור הזמן אם מנתחים או מעצבים תאריכים-שעות ולא רק תאריכים.
