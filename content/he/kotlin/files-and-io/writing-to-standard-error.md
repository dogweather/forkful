---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:08.344067-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC \u05E9\u05D2\u05D9\u05D0\
  \u05D4 \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA (stderr) \u05E7\u05E9\u05D5\
  \u05E8\u05D4 \u05DC\u05D4\u05D5\u05E6\u05D0\u05EA \u05D4\u05D5\u05D3\u05E2\u05D5\
  \u05EA \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D5\u05D0\u05D1\u05D7\u05D5\u05E0\u05D9\
  \u05DD \u05DC\u05D6\u05E8\u05DD \u05E0\u05E4\u05E8\u05D3, \u05E9\u05D5\u05E0\u05D4\
  \ \u05DE\u05D6\u05E8\u05DD \u05D4\u05E4\u05DC\u05D8 \u05D4\u05E1\u05D8\u05E0\u05D3\
  \u05E8\u05D8\u05D9 (stdout), \u05D3\u05D1\u05E8 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8\
  \ \u05D8\u05D9\u05E4\u05D5\u05DC \u05D8\u05D5\u05D1 \u05D9\u05D5\u05EA\u05E8 \u05D1\
  \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05D5\u05E0\u05D9\u05EA\u05D5\u05D7\u2026"
lastmod: 2024-02-19 22:04:58.527573
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05D0\u05DC \u05E9\u05D2\u05D9\u05D0\u05D4\
  \ \u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA (stderr) \u05E7\u05E9\u05D5\u05E8\
  \u05D4 \u05DC\u05D4\u05D5\u05E6\u05D0\u05EA \u05D4\u05D5\u05D3\u05E2\u05D5\u05EA\
  \ \u05E9\u05D2\u05D9\u05D0\u05D4 \u05D5\u05D0\u05D1\u05D7\u05D5\u05E0\u05D9\u05DD\
  \ \u05DC\u05D6\u05E8\u05DD \u05E0\u05E4\u05E8\u05D3, \u05E9\u05D5\u05E0\u05D4 \u05DE\
  \u05D6\u05E8\u05DD \u05D4\u05E4\u05DC\u05D8 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\
  \u05D8\u05D9 (stdout), \u05D3\u05D1\u05E8 \u05E9\u05DE\u05D0\u05E4\u05E9\u05E8 \u05D8\
  \u05D9\u05E4\u05D5\u05DC \u05D8\u05D5\u05D1 \u05D9\u05D5\u05EA\u05E8 \u05D1\u05E9\
  \u05D2\u05D9\u05D0\u05D5\u05EA \u05D5\u05E0\u05D9\u05EA\u05D5\u05D7\u2026"
title: "\u05DB\u05EA\u05D9\u05D1\u05D4 \u05DC\u05E9\u05D2\u05D9\u05D0\u05D4 \u05D4\
  \u05EA\u05E7\u05E0\u05D9\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?

כתיבה אל שגיאה סטנדרטית (stderr) קשורה להוצאת הודעות שגיאה ואבחונים לזרם נפרד, שונה מזרם הפלט הסטנדרטי (stdout), דבר שמאפשר טיפול טוב יותר בשגיאות וניתוח יומנים. תכנתים עושים זאת כדי להקל על ניפוי באגים ולהבטיח שניתן יהיה לזהות ולהפנות הודעות שגיאה בצורה קלה אם יש צורך, תוך שמירה על יומני פלט נקיים או הודעות למשתמש.

## איך לעשות:

בקוטלין, ניתן לכתוב ל-stderr באמצעות `System.err.println()`. שיטה זו דומה ל-`System.out.println()` אך מכוונת את הפלט אל זרם השגיאה הסטנדרטית ולא אל זרם הפלט הסטנדרטי.

```kotlin
fun main() {
    System.err.println("This is an error message!")
}
```

פלט לדוגמה:
```
This is an error message!
```

עבור אפליקציות מורכבות או מובנות יותר, במיוחד אלו שמערבות מסגרות לוגים כמו Logback או SLF4J, ניתן להגדיר רוכז לוגים לכתוב ל-stderr עבור רמות לוג מסוימות (למשל, ERROR).

שימוש ב-SLF4J עם Logback:

1. ראשית, הוסף את ה-API של SLF4J ואת היישום של Logback ל-`build.gradle` שלך:

```groovy
dependencies {
    implementation 'org.slf4j:slf4j-api:1.7.30'
    implementation 'ch.qos.logback:logback-classic:1.2.3'
}
```

2. לאחר מכן, הגדר את Logback (ב-`src/main/resources/logback.xml`) להפנות הודעות ברמת שגיאה ל-stderr:

```xml
<configuration>
    <appender name="STDERR" class="ch.qos.logback.core.ConsoleAppender">
        <target>System.err</target>
        <encoder>
            <pattern>%d{yyyy-MM-dd HH:mm:ss} [%thread] %-5level %logger{36} - %msg%n</pattern>
        </encoder>
    </appender>
    
    <root level="error">
        <appender-ref ref="STDERR" />
    </root>
</configuration>
```

3. לאחר מכן, השתמש ב-SLF4J בקוד הקוטלין שלך כדי לתעד הודעות שגיאה:

```kotlin
import org.slf4j.LoggerFactory

fun main() {
    val logger = LoggerFactory.getLogger("ExampleLogger")
    logger.error("This is an error log message!")
}
```

פלט לדוגמה (ל-stderr):
```
2023-04-01 12:34:56 [main] ERROR ExampleLogger - This is an error log message!
```
