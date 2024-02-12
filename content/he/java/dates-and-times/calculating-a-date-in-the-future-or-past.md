---
title:                "חישוב תאריך בעתיד או בעבר"
date:                  2024-01-20T17:31:24.260746-07:00
model:                 gpt-4-1106-preview
simple_title:         "חישוב תאריך בעתיד או בעבר"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?
חישוב תאריך בעתיד או בעבר זו פעולה שבה אנו מחשבים תאריך חדש מתאריך קיים תוך הוספת או הפחתת ימים, חודשים או שנים. תכנתים עושים זאת בשל צורך לתזמון אירועים, ניתוח נתונים תקופתיים, וכדומה.

## איך לעשות:
התעלול הוא להשתמש ב`java.time`, ספריית הזמן החדשה בJava. קוד לדוגמה:
```java
import java.time.LocalDate;
import java.time.Period;

public class DateCalc {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        Period tenDays = Period.ofDays(10);
        
        LocalDate futureDate = today.plus(tenDays);
        LocalDate pastDate = today.minus(tenDays);

        System.out.println("Today: " + today);
        System.out.println("10 days from now: " + futureDate);
        System.out.println("10 days ago: " + pastDate);
    }
}
```
תוצאה:
```
Today: 2023-04-12
10 days from now: 2023-04-22
10 days ago: 2023-04-02
```

## עיון נוסף
במקום להשתמש ב`java.util.Date` הישן והפחות אינטואיטיבי, החל מ-Java 8 יש לנו את `java.time`. לפני החידוש הזה, היינו צריכים להילחם עם ה-API הישן של פעולות תאריך וזמן. כיום, 'java.time' מכילה אובייקטים כמו `LocalDate`, `LocalTime`, `LocalDateTime`, ו`ZonedDateTime`, שמאפשרים עבודה נוחה עם תאריכים וזמנים בצורה בטיחותית לאזורי זמן. אם אתם צריכים לחשב פערים גדולים של זמן או לעבוד עם זמנים מדויקים למילישניות, עליכם לבחור באובייקט מתאים ב-'java.time'. לא מומלץ להשתמש במחלקות ישנות אלא אם כן אתם עובדים עם קוד שכבר כתוב ומתבסס עליהן.

## גם כדאי לראות
- [Java Date Time API](https://docs.oracle.com/javase/tutorial/datetime/)
- [java.time.LocalDate documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [java.time.Period documentation](https://docs.oracle.com/javase/8/docs/api/java/time/Period.html)
- [Joda-Time](https://www.joda.org/joda-time/) - אף על פי שהמסגרת הזו שימשה רבים לפני יצירת `java.time`, היא עדיין רלוונטית לקוד ישן.
