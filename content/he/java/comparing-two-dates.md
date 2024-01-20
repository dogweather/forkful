---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?

השוואה בין שני תאריכים היא פעולה בה משווים בין שני תאריכים כדי לראות איזה מהם מגיע לפני השני, או האם שני התאריכים הם אותו תאריך. מתכנתים מבצעים את הפעולה הזאת בשלל סיטואציות, כולל בדיקת תוקף של תוקף, ביצוע כמה פעולות על ידי תאריך או שעה מסויימים, ועוד.

## איך לעשות:

חלק זה מדגים איך ניתן להשוות בין שני תאריכים ב-Java. הקוד בתחתית מחפש הבדל בימים בין שני תאריכים:

```Java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class Main {
    public static void main(String[] args) {
        LocalDate date1 = LocalDate.of(2022, 1, 1);
        LocalDate date2 = LocalDate.of(2022, 1, 15);

        long daysBetween = ChronoUnit.DAYS.between(date1, date2);
        System.out.println("ימים בין התאריכים: " + daysBetween);  
    }
}
```
פלט:
```Java
ימים בין התאריכים: 14
```
## צלילה עמוקה:
    
המנגנון להשוואת תאריכים ב-Java הוא חלק מ-API הזמן שנוסף ב-Java 8. קודם לכן, היית צריך להשתמש ב-`java.util.Date` או `java.util.Calendar` שהם גסים לשימוש ולא אינטואיטיביים. ניתן להשוות תאריכים גם על פי שנה, חודש, יום, שעה, דקה, שנייה, מילישנייה ורבות אחרות. 

## ראה גם:

- מדריך Oracle Java ל-API הזמן: [Oracle Java Date-Time Guide](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html)
- רשימת כל היחידות שאפשר להשתמש בהן בכאורה `ChronoUnit`: [Java ChronoUnit enums](https://docs.oracle.com/javase/8/docs/api/java/time/temporal/ChronoUnit.html)
- מאמר טוב מאוד שמתאר את השימוש ב-API החדש: [Using Java's DateTime API](https://www.baeldung.com/java-8-date-time-intro)