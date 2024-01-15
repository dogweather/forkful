---
title:                "קבלת התאריך הנוכחי"
html_title:           "Java: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## למה

קבלת התאריך הנוכחי חשובה בעיקר כשמתכנתים רוצים ליצור יישומים דינמיים שנתמכים על פני זמן. זה יכול להיות שימושי גם כאשר אנשים רוצים להציג את התאריך הנוכחי למשתמשים או להשתמש בו באופן כללי בתור חלק מיכולות האפליקציה.

## כיצד לעשות זאת

כדי לקבל את התאריך הנוכחי בשפת ג'אווה, ניתן להשתמש במחלקה הפנימית "Date" או במחלקת "Calendar". להלן דוגמאות של שימוש בשתי השיטות:

```Java
import java.util.Date;
import java.util.Calendar;

// עותק מתאריך נוכחי משמש כבסיס
Date currentDate = new Date();
System.out.println(currentDate);

// עותק מתאריך נוכחי משמש לטפל בחלקים שונים של תאריך כגון יום וחודש
Calendar calendar = Calendar.getInstance();
System.out.println(calendar.get(Calendar.MONTH) + 1);
System.out.println(calendar.get(Calendar.DATE));
```

לתוצאות הדוגמאות הנלוות נרשמים:

```
Wed Jun 23 14:57:41 IST 2021
6
23
```

## חקר מעמיק בקבלת התאריך הנוכחי

במאמר זה ראינו כיצד לקבל את התאריך הנוכחי בשפת ג'אווה באמצעות שתי שיטות נפוצות. אם תרצה ללמוד עוד על אופן התפקוד של מחלקות Date ו-Calendar ועל המתודות שניתן להשתמש בהן לעיבוד תאריך, אתה מוזמן לקרוא את תיעוד השפה הרשמי של ג'אווה.

## ראה גם

- [תיעוד של ג'אווה-אופיציה ראשונית למתכנתים חדשים](https://www.oracle.com/java/technologies/learn-java.html)
- [מדריך לקבלת התאריך הנוכחי בג'אווה](https://www.baeldung.com/java-get-current-date-time)