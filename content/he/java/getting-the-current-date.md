---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?

לקבלת התאריך הנוכחי היא פעולה שבה משתמשים לוח זמנים חדירגעי מהמחשב שלך על מנת לקבוע את התאריך והשעה הנוכחיים. תכנתים עשויים לזקוק לכך כדי לתייג יצירת נתונים, לקבוע מועדים מסוימים או להגביר את הבטיחות על ידי שימוש בחותמות זמן.

## איך לעשות:

להלן קטעי הקוד שמדגימים איך להשיג את התאריך והשעה הנוכחיים ב-Java:

```Java
import java.time.LocalDateTime;

public class Main {
  public static void main(String[] args) {
    LocalDateTime currentDate = LocalDateTime.now(); 
    System.out.println("The current date and time is: " + currentDate);
  }
}
```
הפלט מהקוד יהיה משהו כמו:
```
The current date and time is: 2022-04-10T13:48:03.711
```
## צוללים לעומק

מאז הוורסיה 8 של Java, מספר פונקציות חדשות התווספו לעבודה עם תאריכים וזמנים, המקלות על מחלקות התאריך המסורתיות. חלופות לLocalDateTime כוללות LocalDate (שישים את התאריך בלבד ללא השעה) וLocalTime (שישים את השעה בלבד ללא התאריך).

הרעיון מאחורי של קבלת התאריך הנוכחי הוא לשלוף את זמן המערכת ולהמירו לתאריך ספציפי. תהליך זה משתמש בשעון המערכת, שהוא ממשק שמאפשר למתכנת לגשת לשעון הנוכחי של המחשב.

## ראה גם

1. [Java API documentation on LocalDateTime](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/time/LocalDateTime.html)
3. [Accessing the System's Current Date/Time](https://docs.oracle.com/javase/tutorial/datetime/iso/system.html)