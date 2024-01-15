---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "Java: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## למה
כשמפתחים יישומי תוכנה ב-Java, לפעמים נדרש לחשב תאריך בעתיד או בעבר. החישוב של תאריך כזה יכול להיות מאתגר ומצריך שימוש באלגוריתמים מתקדמים. כתבה זו תסביר כיצד לחשב תאריך בעתיד או בעבר באמצעות שפת התכנות Java.

## איך לעשות
תחילה, אנו צריכים להתחבר לכמה מחלקות מובנות של Java שמספקות כלים לחישוב תאריכים. אחת מהן היא המחלקה java.util.Calendar, שמאפשרת לנו ליצור אובייקט תאריך ולבצע עליו פעולות כמו הוספת ימים או חיסור ימים. הנה דוגמא לכיצד לחשב תאריך בעתיד:

```Java
// ייבוא המחלקות הנדרשות
import java.util.Calendar;

// יצירת אובייקט Calendar
Calendar cal = Calendar.getInstance();

// הוספת 5 ימים לתאריך הנוכחי
cal.add(Calendar.DAY_OF_MONTH, 5);

// הדפסת התאריך שנוצר
System.out.println("תאריך בעתיד: " + cal.getTime());
```

הפלט של הקוד הנ"ל יהיה:

```
תאריך בעתיד: Tue Dec 08 16:44:11 IDT 2020
```

לחישוב תאריך בעבר, ניתן להשתמש בפעולה ההפוכה של add, המכילה את המחרוזת "דבר, כמה". לדוגמא:

```Java
cal.add(Calendar.DAY_OF_MONTH, -5);
```

כדי לחשב תאריך בדיוק בתאריך מסוים, ניתן להשתמש בפעולת set. לדוגמא, לחישוב תאריך בעבר:

```Java
// יצירת אובייקט Calendar המייצג את התאריך הנוכחי
Calendar cal = Calendar.getInstance();

// הגדרת התאריך ליום 5 מאוקטובר 2020
cal.set(2020, Calendar.OCTOBER, 5);

// הוספת 5 ימים לתאריך הנוכחי
cal.add(Calendar.DAY_OF_MONTH, 5);

// הדפסת התאריך שנוצר
System.out.println("תאריך בעתיד: " + cal.getTime());
```

הפלט של הקוד הנ"ל יהיה:

```
תאריך בעתיד: Sun Oct 11 16:45:07 IDT 2020
```