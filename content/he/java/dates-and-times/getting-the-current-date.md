---
title:                "קבלת התאריך הנוכחי"
aliases:
- /he/java/getting-the-current-date/
date:                  2024-02-03T19:10:21.239319-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
השגת התאריך הנוכחי בג'אווה היא פעולה יסודית המאפשרת לתכנתים לנהל אובייקטים של תאריכים לצורך פעולות כמו רישום לוגים, חישובי תאריכים, ותנאים התלויים בזמן. זה חיוני ביישומים בהם מעקב, תזמון וניתוח נתונים זמניים הם קריטיים.

## איך לעשות:
ג'אווה מציעה מספר דרכים לקבל את התאריך הנוכחי, באמצעות הן המחלקה הוותיקה `java.util.Date` וגם החבילה החדשה יותר `java.time` (שהוצגה בג'אווה 8) אשר היא יותר גמישה ואינטואיטיבית.

### באמצעות `java.time.LocalDate`
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // דוגמה לפלט: 2023-04-01
    }
}
```
### באמצעות `java.time.LocalDateTime`
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // דוגמה לפלט: 2023-04-01T12:34:56.789
    }
}
```
### באמצעות `java.util.Date` (מורשת)
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // דוגמה לפלט: Sat Apr 01 12:34:56 BST 2023
    }
}
```
### שימוש בספריה צד שלישי: Joda-Time
לפני ג'אווה 8, Joda-Time הייתה התקן הבלתי מסורתי לתאריך ושעה בג'אווה. אם אתה עובד על מערכות מורשת או שיש לך עדפה ל-Joda-Time, הנה איך אתה יכול להשתמש בה לקבלת התאריך הנוכחי:
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // דוגמה לפלט: 2023-04-01
    }
}
```
**שימו לב:** למרות ש-`java.util.Date` ו-Joda-Time עדיין משמשים, החבילה `java.time` מומלצת לפרויקטים חדשים בשל אי-השתנותה וממשק ה-API הכוללני שלה לניהול תאריכים וזמנים.
