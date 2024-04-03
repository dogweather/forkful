---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:08.943603-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4\
  \ \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA (stderr) \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05D4\u05D6\u05E8\u05DE\u05EA \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05E9\
  \u05D2\u05D9\u05D0\u05D4 \u05D5\u05E0\u05EA\u05D5\u05E0\u05D9 \u05D0\u05D1\u05D7\
  \u05D5\u05DF \u05DC\u05E7\u05D5\u05E0\u05E1\u05D5\u05DC \u05D0\u05D5 \u05DC\u05D8\
  \u05E8\u05DE\u05D9\u05E0\u05DC. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\
  \u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\
  \u05D4\u05E4\u05E8\u05D9\u05D3 \u05D0\u05EA \u05D4\u05DE\u05D9\u05D3\u05E2 \u05E2\
  \u05DC \u05D4\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05DE\u05D4\u05E4\u05DC\u05D8\
  \u2026"
lastmod: '2024-03-13T22:44:39.158308-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05E1\
  \u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA (stderr) \u05DB\u05D5\u05DC\u05DC\u05EA\
  \ \u05D4\u05D6\u05E8\u05DE\u05EA \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA \u05E9\u05D2\
  \u05D9\u05D0\u05D4 \u05D5\u05E0\u05EA\u05D5\u05E0\u05D9 \u05D0\u05D1\u05D7\u05D5\
  \u05DF \u05DC\u05E7\u05D5\u05E0\u05E1\u05D5\u05DC \u05D0\u05D5 \u05DC\u05D8\u05E8\
  \u05DE\u05D9\u05E0\u05DC."
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
weight: 25
---

## מה ולמה?
כתיבה לשגיאה סטנדרטית (stderr) כוללת הזרמת הודעות שגיאה ונתוני אבחון לקונסול או לטרמינל. מתכנתים עושים זאת על מנת להפריד את המידע על השגיאות מהפלט הסטנדרטי (stdout), מה שמקל על תהליכי ניפוי באגים וניתוח יומנים.

## איך לעשות:

### פלט stderr בסיסי בג'אווה
ג'אווה מספקת דרך ישירה לכתוב ל stderr באמצעות השימוש ב `System.err.print()` או `System.err.println()`. כך אתם עושים זאת:

```java
public class StdErrExample {
    public static void main(String[] args) {
        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            System.err.println("Error: Cannot divide by zero.");
        }
    }
}
```

דוגמא לפלט:

```
Error: Cannot divide by zero.
```

זה ידפיס ישירות את הודעת השגיאה לזרם השגיאה הסטנדרטית.

### שימוש ב Logger לטיפול מתקדם יותר בשגיאות
ליישומים שדורשים טיפול בשגיאות וברישום מתקדמים יותר, שימוש בספריית רישום כמו SLF4J עם Logback או Log4J2 הוא נפוץ. זה מאפשר גמישות רבה יותר בניהול פלט השגיאות, כולל הפניית קבצים, סינון ועיצוב.

#### דוגמא עם Logback

ראשית, הוסיפו את התלות לLogback לקובץ `pom.xml` שלכם (Maven) או `build.gradle` (Gradle). לMaven:

```xml
<dependency>
    <groupId>ch.qos.logback</groupId>
    <artifactId>logback-classic</artifactId>
    <version>1.2.3</version>
</dependency>
```

לאחר מכן, תוכלו להשתמש בקוד הבא כדי לרשום שגיאות:

```java
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LoggerExample {
    private static final Logger logger = LoggerFactory.getLogger(LoggerExample.class);
    
    public static void main(String[] args) {
        try {
            int result = 10 / 0;
        } catch (ArithmeticException e) {
            logger.error("Error: Cannot divide by zero.", e);
        }
    }
}
```

זה יפלוט את הודעת השגיאה יחד עם עקבות המחסה לקונסול או לקובץ, בהתאם להגדרת Logback.

השימוש במסגרות רישום כמו Logback מספק שליטה רבה יותר על טיפול בשגיאות, דבר שמקל על ניהול יישומים ומערכות גדולים.
