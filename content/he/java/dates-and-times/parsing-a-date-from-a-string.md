---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:39.771195-07:00
description: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05DE\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DB\u05D5\u05DC\u05DC \u05D4\u05DE\
  \u05E8\u05EA \u05D4\u05D9\u05D9\u05E6\u05D5\u05D2 \u05D4\u05D8\u05E7\u05E1\u05D8\
  \u05D5\u05D0\u05DC\u05D9 \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05E9\
  \u05E2\u05D4 \u05DC\u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 \u05DE\u05E1\u05D5\
  \u05D2 `Date` \u05D0\u05D5 \u05DC\u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 `LocalDateTime`\
  \ \u05DE\u05D5\u05D3\u05E8\u05E0\u05D9 \u05D9\u05D5\u05EA\u05E8. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05E0\u05D4\u05DC,\u2026"
lastmod: '2024-03-13T22:44:39.146597-06:00'
model: gpt-4-0125-preview
summary: "\u05E4\u05D9\u05E2\u05E0\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DB\u05D5\u05DC\u05DC \u05D4\u05DE\u05E8\
  \u05EA \u05D4\u05D9\u05D9\u05E6\u05D5\u05D2 \u05D4\u05D8\u05E7\u05E1\u05D8\u05D5\
  \u05D0\u05DC\u05D9 \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05E9\u05E2\
  \u05D4 \u05DC\u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 \u05DE\u05E1\u05D5\u05D2\
  \ `Date` \u05D0\u05D5 \u05DC\u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 `LocalDateTime`\
  \ \u05DE\u05D5\u05D3\u05E8\u05E0\u05D9 \u05D9\u05D5\u05EA\u05E8. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\
  \u05D3\u05D9 \u05DC\u05E0\u05D4\u05DC,\u2026"
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
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
