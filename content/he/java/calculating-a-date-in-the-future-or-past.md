---
title:    "Java: חישוב תאריך בעתיד או בעבר"
keywords: ["Java"]
---

{{< edit_this_page >}}

## למה

כתיבת קוד עם יכולת לחשב תאריך בעתיד או בעבר יכולה להיות שימושית במגוון מקרים, כגון יישומי תאריכים או ניהול של מועדי תשלום. 

## איך לעשות זאת

כתיבת קוד ב-Java יכולה לעזור לנו לחשב תאריך נתון בעתיד או בעבר. לדוגמה, ננסה לחשב את התאריך 30 ימים מהיום:

```java
import java.time.LocalDate;

LocalDate today = LocalDate.now();
LocalDate futureDate = today.plusDays(30);

System.out.println("התאריך המבוקש הוא " + futureDate);
```

פלט:

התאריך המבוקש הוא 2021-02-24

לחישוב תאריך בעבר, נשתמש בפעולת "minus" במקום "plus":

```java
import java.time.LocalDate;

LocalDate today = LocalDate.now();
LocalDate pastDate = today.minusMonths(6);

System.out.println("התאריך המבוקש הוא " + pastDate);
```

פלט:

התאריך המבוקש הוא 2020-08-24

## טיול עמוק

החישובים שביצענו נעשו על ידי שימוש בספריית התאריכים המובנית של Java, LocalDate. ניתן למצוא מגוון פעולות נוספות בספרייה זו לחישוב וניהול תאריכים בצורה יעילה ונוחה.

## ראו גם

- [מדריך לתאריכים ב-Java](https://www.geeksforgeeks.org/date-class-java-examples/)
- [תיעוד רשמי מפורט על LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [הספריה המתקדמת של Joda-Time לניהול תאריכים](https://www.joda.org/joda-time/index.html)