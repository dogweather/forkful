---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:39.771195-07:00
description: ''
lastmod: '2024-04-05T21:59:51.916700-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

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
