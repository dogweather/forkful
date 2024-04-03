---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:03.562936-07:00
description: "\u05D1\u05EA\u05DB\u05E0\u05D5\u05EA, \u05D4\u05E9\u05D2\u05EA \u05D4\
  \u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9 \u05D4\u05D9\
  \u05D0 \u05DE\u05E9\u05D9\u05DE\u05D4 \u05D9\u05E1\u05D5\u05D3\u05D9\u05EA \u05E9\
  \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DE\u05E4\u05EA\u05D7\u05D9\u05DD\
  \ \u05DC\u05D2\u05E9\u05EA, \u05DC\u05D4\u05E6\u05D9\u05D2 \u05D0\u05D5 \u05DC\u05E9\
  \u05E0\u05D5\u05EA \u05D0\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9 \u05D1\u05EA\u05D5\u05DA \u05D4\u05D9\u05D9\u05E9\u05D5\
  \u05DE\u05D9\u05DD \u05E9\u05DC\u05D4\u05DD. \u05D9\u05DB\u05D5\u05DC\u05EA \u05D6\
  \u05D5 \u05E7\u05E8\u05D9\u05D8\u05D9\u05EA \u05DC\u05DB\u05DC \u05D3\u05D1\u05E8\
  , \u05D4\u05D7\u05DC \u05DE\u05EA\u05D9\u05E2\u05D5\u05D3\u2026"
lastmod: '2024-03-13T22:44:39.288890-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05EA\u05DB\u05E0\u05D5\u05EA, \u05D4\u05E9\u05D2\u05EA \u05D4\u05EA\
  \u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9 \u05D4\u05D9\u05D0\
  \ \u05DE\u05E9\u05D9\u05DE\u05D4 \u05D9\u05E1\u05D5\u05D3\u05D9\u05EA \u05E9\u05DE\
  \u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DE\u05E4\u05EA\u05D7\u05D9\u05DD \u05DC\
  \u05D2\u05E9\u05EA, \u05DC\u05D4\u05E6\u05D9\u05D2 \u05D0\u05D5 \u05DC\u05E9\u05E0\
  \u05D5\u05EA \u05D0\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\u05D5\
  \u05DB\u05D7\u05D9 \u05D1\u05EA\u05D5\u05DA \u05D4\u05D9\u05D9\u05E9\u05D5\u05DE\
  \u05D9\u05DD \u05E9\u05DC\u05D4\u05DD."
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
weight: 29
---

## איך לעשות:


### באמצעות קוטלין סטנדרטי
לקוטלין אין API ייחודי משלו לתאריך ושעה, אך היא מתבססת על ספריית הסטנדרט של Java לפונקציונליות זו. הנה איך אפשר להשיג את התאריך הנוכחי:

```kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("תאריך היום: $today")
}
```

**פלט לדוגמה:**
```
תאריך היום: 2023-04-05
```

### באמצעות java.util.Date
לפעולות שדורשות גם תאריך וגם שעה, עשוי להיות מועדף `java.util.Date`.

```kotlin
import java.util.Date

fun main() {
    val currentDate = Date()
    println("תאריך ושעה נוכחיים: $currentDate")
}
```

**פלט לדוגמה:**
```
תאריך ושעה נוכחיים: Wed Apr 05 15:20:45 GMT 2023
```

### באמצעות ספריית Joda-Time
לפני ש-Java 8 הציגה את ה-API החדש לתאריך ושעה, Joda-Time הייתה התקן הדה-פקטו לפעולות תאריך ושעה ב-Java וב-Kotlin. למרות שהיא כבר לא נדרשת לרוב הפרויקטים, ייתכן שחלק עדיין ישתמשו בה מסיבות של שמירה על קוד קיים או העדפה אישית.

הוסף את ספריית Joda-Time לקובץ build.gradle של הפרויקט שלך:
```
implementation 'joda-time:joda-time:2.10.10'
```

```kotlin
import org.joda.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("תאריך היום: $today")
}
```

**פלט לדוגמה:**
```
תאריך היום: 2023-04-05
```

### באמצעות ThreeTenABP עבור אנדרואיד
לפיתוח אפליקציות אנדרואיד, מומלץ להשתמש ב-backport של Java Time API באמצעות פרויקט ה-ThreeTen Android Backport עבור גרסאות לפני Android API Level 26.

הוסף את התלות לקובץ build.gradle של האפליקציה שלך:
```
implementation 'com.jakewharton.threetenabp:threetenabp:1.3.1'
```

אתחל אותו במחלקת ה-Application שלך:
```kotlin
import android.app.Application
import com.jakewharton.threetenabp.AndroidThreeTen

class MyApp : Application() {
    override fun onCreate() {
        super.onCreate()
        AndroidThreeTen.init(this)
    }
}
```

לאחר מכן, אתה יכול להשתמש בו כך:
```kotlin
import org.threeten.bp.LocalDate

fun main() {
    val today = LocalDate.now()
    println("תאריך היום: $today")
}
```

**פלט לדוגמה:**
```
תאריך היום: 2023-04-05
```
