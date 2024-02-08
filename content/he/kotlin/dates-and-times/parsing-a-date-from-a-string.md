---
title:                "פרסום תאריך ממחרוזת"
date:                  2024-02-03T19:14:59.019245-07:00
model:                 gpt-4-0125-preview
simple_title:         "פרסום תאריך ממחרוזת"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
