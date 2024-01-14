---
title:                "Java: המרת תאריך למחרוזת"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

למה לעסוק בהמרת תאריך למחרוזת? יתרונותיו של המרת תאריך למחרוזת הן הגמישות והיכולת לנטרל כלליות מסוימות במיצוב התאריך.

## איך לעשות

בקוד הבא אתם תמצאו דוגמאות להמרת תאריך למחרוזת ולפלט משובק:

```Java
import java.util.Date;
import java.text.SimpleDateFormat;

class DateToString {
   public static void main(String[] args) {
      try {
         // יצירת אובייקט תאריך
         Date date = new Date();

         // הגדרת פורמט של התאריך כמחרוזת
         SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy HH:mm:ss");

         // המרת התאריך למחרוזת והדפסת התוצאה
         System.out.println(sdf.format(date));
      } catch (Exception e) {
         System.out.println("שגיאה בהמרת תאריך");
      }
   }
}
```

פלט:

```
31/10/2021 16:35:20
```

## יעדר

העמקת המבוא יכולה לנסות להפוך את התאריך למחרוזת בפורמטים שונים, כגון רצף של תווים (למשל "31/10/21"), או שיבוצעו טכניקות נוספות כגון הוספת תאריך נוסף. בנוסף, ישנן פונקציות מוכנות שניתן להשתמש בהן כדי להמיר את התאריך למחרוזת בפורמטים מתקדמים יותר עם תמיכה בארכיטקטורות מקור, כגון פקודות על ציוד חוץ מערכי ניהול מחשבים שונים.

## ראה גם

- [תיעוד Oracle על המרת תאריך למחרוזת](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [מדריך מבוא MS להמרת תאריכים למחרוזת](https://msdn.microsoft.com/he-il/library/ms187928.aspx)