---
title:                "כתיבה לשגיאה התקנית"
aliases:
- /he/java/writing-to-standard-error.md
date:                  2024-02-03T19:34:08.943603-07:00
model:                 gpt-4-0125-preview
simple_title:         "כתיבה לשגיאה התקנית"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
