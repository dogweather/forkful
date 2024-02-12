---
title:                "קבלת התאריך הנוכחי"
aliases:
- /he/kotlin/getting-the-current-date/
date:                  2024-02-03T19:11:03.562936-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
בתכנות, השגת התאריך הנוכחי היא משימה יסודית שמאפשרת למפתחים לגשת, להציג או לשנות את התאריך הנוכחי בתוך היישומים שלהם. יכולת זו קריטית לכל דבר, החל מתיעוד וזימון אירועים ועד חישובים המבוססים על תאריכים.

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
