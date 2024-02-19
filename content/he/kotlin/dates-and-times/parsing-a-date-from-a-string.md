---
aliases:
- /he/kotlin/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:59.019245-07:00
description: "\u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05DB\u05D5\u05DC\u05DC \u05D4\u05DE\u05E8\
  \u05EA \u05D8\u05E7\u05E1\u05D8 \u05DC\u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8\
  \ \u05DE\u05E1\u05D5\u05D2 \u05EA\u05D0\u05E8\u05D9\u05DA. \u05E4\u05E2\u05D5\u05DC\
  \u05D4 \u05D6\u05D5 \u05D4\u05D9\u05D0 \u05D9\u05E1\u05D5\u05D3\u05D9\u05EA \u05E2\
  \u05D1\u05D5\u05E8 \u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\u05DD \u05E9\u05DE\u05EA\
  \u05E7\u05E9\u05E8\u05D9\u05DD \u05E2\u05DD \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\
  \u05DD \u05E9\u05D4\u05D5\u05D6\u05E0\u05D5 \u05E2\u05DC \u05D9\u05D3\u05D9 \u05DE\
  \u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D0\u05D5 \u05E9\u05D4\u05EA\u05E7\u05D1\
  \u05DC\u05D5 \u05DE\u05E1\u05D8\u05D9\u05DD\u2026"
lastmod: 2024-02-18 23:08:52.806631
model: gpt-4-0125-preview
summary: "\u05E0\u05D9\u05EA\u05D5\u05D7 \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05DB\u05D5\u05DC\u05DC \u05D4\u05DE\u05E8\u05EA\
  \ \u05D8\u05E7\u05E1\u05D8 \u05DC\u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8 \u05DE\
  \u05E1\u05D5\u05D2 \u05EA\u05D0\u05E8\u05D9\u05DA. \u05E4\u05E2\u05D5\u05DC\u05D4\
  \ \u05D6\u05D5 \u05D4\u05D9\u05D0 \u05D9\u05E1\u05D5\u05D3\u05D9\u05EA \u05E2\u05D1\
  \u05D5\u05E8 \u05D9\u05D9\u05E9\u05D5\u05DE\u05D9\u05DD \u05E9\u05DE\u05EA\u05E7\
  \u05E9\u05E8\u05D9\u05DD \u05E2\u05DD \u05EA\u05D0\u05E8\u05D9\u05DB\u05D9\u05DD\
  \ \u05E9\u05D4\u05D5\u05D6\u05E0\u05D5 \u05E2\u05DC \u05D9\u05D3\u05D9 \u05DE\u05E9\
  \u05EA\u05DE\u05E9\u05D9\u05DD \u05D0\u05D5 \u05E9\u05D4\u05EA\u05E7\u05D1\u05DC\
  \u05D5 \u05DE\u05E1\u05D8\u05D9\u05DD\u2026"
title: "\u05E4\u05E8\u05E1\u05D5\u05DD \u05EA\u05D0\u05E8\u05D9\u05DA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
ניתוח תאריך ממחרוזת כולל המרת טקסט לאובייקט מסוג תאריך. פעולה זו היא יסודית עבור יישומים שמתקשרים עם תאריכים שהוזנו על ידי משתמשים או שהתקבלו מסטים חיצוניים, מה שמאפשר טיפול קל ופורמט מותאם לפי הצורך.

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
