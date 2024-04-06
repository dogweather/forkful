---
date: 2024-01-20 17:37:22.923137-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05DE\u05E8\
  \u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\u05DE\u05D7\u05E8\u05D5\
  \u05D6\u05D5\u05EA \u05D4\u05D9\u05D0 \u05D7\u05DC\u05E7 \u05DE\u05D4\u05D4\u05EA\
  \u05DE\u05D5\u05D3\u05D3\u05D5\u05EA \u05E2\u05DD \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05DB\u05D1\u05E8 \u05DE\u05D0\u05D6 \u05E9\u05D4\u05EA\u05D7\u05D9\u05DC\
  \u05D5 \u05DC\u05EA\u05DB\u05E0\u05EA \u05D1-Java. \u05D4-API \u05E9\u05DC `java.util.Date`\
  \ \u05D4\u05D0\u05DE\u05E0\u05DD \u05D9\u05E9\u05DF \u05D5\u05E4\u05E2\u05DE\u05D9\
  \u05DD \u05E8\u05D1\u05D5\u05EA \u05DE\u05D1\u05DC\u05D1\u05DC, \u05D0\u05DA \u05DE\
  \u05D0\u05D6\u2026"
lastmod: '2024-04-05T21:53:40.380276-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D4\u05D9\u05D0 \u05D7\u05DC\u05E7\
  \ \u05DE\u05D4\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3\u05D5\u05EA \u05E2\u05DD \u05E0\
  \u05EA\u05D5\u05E0\u05D9\u05DD \u05DB\u05D1\u05E8 \u05DE\u05D0\u05D6 \u05E9\u05D4\
  \u05EA\u05D7\u05D9\u05DC\u05D5 \u05DC\u05EA\u05DB\u05E0\u05EA \u05D1-Java."
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
weight: 28
---

## איך לעשות:
```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateToStringExample {
    public static void main(String[] args) {
        LocalDate date = LocalDate.now(); // יוצרים אובייקט תאריך של היום.
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy"); // מגדירים פורמט.
        String dateString = date.format(formatter); // ממירים את התאריך למחרוזת.
        
        System.out.println(dateString); // פלט: 31/12/2023, לדוגמא.
    }
}
```

## צלילה עמוקה:
המרת תאריכים למחרוזות היא חלק מההתמודדות עם נתונים כבר מאז שהתחילו לתכנת ב-Java. ה-API של `java.util.Date` האמנם ישן ופעמים רבות מבלבל, אך מאז Java 8, תוכניתנים מעדיפים להשתמש ב-API של `java.time`, הכולל כלים רבים ונוחים יותר לניהול זמן ותאריכים. בנוסף ל-`DateTimeFormatter`, ניתן להשתמש ב-`SimpleDateFormat` אך שימו לב – הוא אינו חסין לבעיות עם תיים זון ומצבים מרובי-חוטים. תמיד כדאי לקרוא על מנגנונים נוספים ולוודא מה הכי מתאים למקרה הספציפי שלכם.

## ראו גם:
- [תיעוד לספריית `java.time`](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [מדריך ל-`DateTimeFormatter`](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
