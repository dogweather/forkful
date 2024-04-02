---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:21.239319-07:00
description: "\u05D4\u05E9\u05D2\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\
  \u05E0\u05D5\u05DB\u05D7\u05D9 \u05D1\u05D2'\u05D0\u05D5\u05D5\u05D4 \u05D4\u05D9\
  \u05D0 \u05E4\u05E2\u05D5\u05DC\u05D4 \u05D9\u05E1\u05D5\u05D3\u05D9\u05EA \u05D4\
  \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD\
  \ \u05DC\u05E0\u05D4\u05DC \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8\u05D9\u05DD\
  \ \u05E9\u05DC \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\u05E6\u05D5\u05E8\
  \u05DA \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05DB\u05DE\u05D5 \u05E8\u05D9\u05E9\
  \u05D5\u05DD \u05DC\u05D5\u05D2\u05D9\u05DD, \u05D7\u05D9\u05E9\u05D5\u05D1\u05D9\
  \ \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD, \u05D5\u05EA\u05E0\u05D0\u05D9\u05DD\
  \ \u05D4\u05EA\u05DC\u05D5\u05D9\u05D9\u05DD \u05D1\u05D6\u05DE\u05DF.\u2026"
lastmod: '2024-03-13T22:44:39.148376-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E9\u05D2\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9 \u05D1\u05D2'\u05D0\u05D5\u05D5\u05D4 \u05D4\u05D9\u05D0\
  \ \u05E4\u05E2\u05D5\u05DC\u05D4 \u05D9\u05E1\u05D5\u05D3\u05D9\u05EA \u05D4\u05DE\
  \u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DC\
  \u05E0\u05D4\u05DC \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8\u05D9\u05DD \u05E9\
  \u05DC \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05DC\u05E6\u05D5\u05E8\u05DA\
  \ \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05DB\u05DE\u05D5 \u05E8\u05D9\u05E9\u05D5\
  \u05DD \u05DC\u05D5\u05D2\u05D9\u05DD, \u05D7\u05D9\u05E9\u05D5\u05D1\u05D9 \u05EA\
  \u05D0\u05E8\u05D9\u05DB\u05D9\u05DD, \u05D5\u05EA\u05E0\u05D0\u05D9\u05DD \u05D4\
  \u05EA\u05DC\u05D5\u05D9\u05D9\u05DD \u05D1\u05D6\u05DE\u05DF.\u2026"
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
weight: 29
---

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
