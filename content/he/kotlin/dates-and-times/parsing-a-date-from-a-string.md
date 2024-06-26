---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:59.019245-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : Kotlin \u05EA\u05D5\u05DE\u05DA \u05D1\u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D0\
  \u05E8\u05D9\u05DB\u05D9\u05DD \u05D3\u05E8\u05DA \u05D4\u05D7\u05D1\u05D9\u05DC\
  \u05D4 `java.time`, \u05E9\u05D4\u05D5\u05E6\u05D2\u05D4 \u05D1Java 8. \u05D4\u05E0\
  \u05D4 \u05D2\u05D9\u05E9\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4 \u05D1\u05D0\u05DE\
  \u05E6\u05E2\u05D5\u05EA `LocalDateTime` \u05D5\u05EA\u05D1\u05E0\u05D9\u05EA \u05DE\
  \u05E1\u05D5\u05D9\u05DE\u05EA."
lastmod: '2024-03-13T22:44:39.287096-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u05EA\u05D5\u05DE\u05DA \u05D1\u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\
  \u05D0\u05E8\u05D9\u05DB\u05D9\u05DD \u05D3\u05E8\u05DA \u05D4\u05D7\u05D1\u05D9\
  \u05DC\u05D4 `java.time`, \u05E9\u05D4\u05D5\u05E6\u05D2\u05D4 \u05D1Java 8."
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 30
---

## איך לעשות זאת:
Kotlin תומך בניתוח תאריכים דרך החבילה `java.time`, שהוצגה בJava 8. הנה גישה פשוטה באמצעות `LocalDateTime` ותבנית מסוימת:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    התאריך = parseDateFromString(dateString)
    println(date)  // פלט: 2023-04-01T12:00
}
```

למגוון רחב יותר, או לטיפול בתאריכים ממקורות חיצוניים כמו APIים, אפשר להשתמש בספרייה צד שלישי כמו Joda-Time (אם כי זה פחות נפוץ עכשיו עם `java.time` שהוא עמיד). עם זאת, ההעדפה היא לשמור על השיטה המודרנית שמספקת ה-JDK עבור רוב יישומי Kotlin.

לניתוח תאריך בKotlin ללא שימוש בספריות צד שלישי, ניתן גם להשתמש במחלקה `SimpleDateFormat` עבור גרסאות לפני Java 8 או רמות API של Android שאין להן תמיכה ב`java.time`:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    התאריך = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // הפלט ישתנה בהתאם לאזור הזמן שלך, לדוגמה, Sat Apr 01 12:00:00 GMT 2023
}
```

זכור לקבוע תמיד את אזור הזמן כאשר אתה עובד עם `SimpleDateFormat` כדי להימנע מהסטות בלתי צפויות בתאריכים שניתחת.
