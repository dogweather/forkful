---
aliases:
- /he/java/converting-a-date-into-a-string/
date: 2024-01-20 17:37:22.923137-07:00
description: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-Java \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\
  \u05D9\u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D5 \u05DE\u05E9\u05E0\u05D9\u05DD\
  \ \u05D0\u05EA \u05E4\u05D5\u05E8\u05DE\u05D8 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05DC\u05D8\u05E7\u05E1\u05D8. \u05D6\u05D4 \u05E0\u05E2\u05E9\u05D4 \u05DB\u05D3\
  \u05D9 \u05DC\u05D0\u05E4\u05E9\u05E8 \u05EA\u05E6\u05D5\u05D2\u05D4 \u05D9\u05D3\
  \u05D9\u05D3\u05D5\u05EA\u05D9\u05EA \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D0\
  \u05D5 \u05DC\u05E4\u05D5\u05E8\u05DE\u05D8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\
  \ \u05DC\u05E9\u05DE\u05D9\u05E8\u05D4 \u05D5\u05E9\u05D9\u05EA\u05D5\u05E3."
lastmod: 2024-02-18 23:08:52.720992
model: gpt-4-1106-preview
summary: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05D1-Java \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\
  \u05DA \u05E9\u05D1\u05D5 \u05D0\u05E0\u05D5 \u05DE\u05E9\u05E0\u05D9\u05DD \u05D0\
  \u05EA \u05E4\u05D5\u05E8\u05DE\u05D8 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\
  \u05D8\u05E7\u05E1\u05D8. \u05D6\u05D4 \u05E0\u05E2\u05E9\u05D4 \u05DB\u05D3\u05D9\
  \ \u05DC\u05D0\u05E4\u05E9\u05E8 \u05EA\u05E6\u05D5\u05D2\u05D4 \u05D9\u05D3\u05D9\
  \u05D3\u05D5\u05EA\u05D9\u05EA \u05DC\u05DE\u05E9\u05EA\u05DE\u05E9 \u05D0\u05D5\
  \ \u05DC\u05E4\u05D5\u05E8\u05DE\u05D8 \u05E0\u05EA\u05D5\u05E0\u05D9\u05DD \u05DC\
  \u05E9\u05DE\u05D9\u05E8\u05D4 \u05D5\u05E9\u05D9\u05EA\u05D5\u05E3."
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
המרת תאריך למחרוזת ב-Java היא תהליך שבו אנו משנים את פורמט התאריך לטקסט. זה נעשה כדי לאפשר תצוגה ידידותית למשתמש או לפורמט נתונים לשמירה ושיתוף.

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
