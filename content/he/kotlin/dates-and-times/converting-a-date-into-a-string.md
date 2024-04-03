---
date: 2024-01-20 17:38:02.520806-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05E7\u05D5\u05D3\
  \ Kotlin \u05DC\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA."
lastmod: '2024-03-13T22:44:39.290663-06:00'
model: gpt-4-1106-preview
summary: "\u05E7\u05D5\u05D3 Kotlin \u05DC\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\
  \u05D9\u05DA \u05DC\u05DE\u05D7\u05E8\u05D5\u05D6\u05EA."
title: "\u05D4\u05DE\u05E8\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05DC\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA"
weight: 28
---

## איך לעשות:
קוד Kotlin להמרת תאריך למחרוזת:

```Kotlin
import java.text.SimpleDateFormat
import java.util.*

fun formatDateToString(date: Date, pattern: String): String {  // 1
    val formatter = SimpleDateFormat(pattern, Locale.getDefault())  // 2
    return formatter.format(date)  // 3
}

fun main() {
    val date = Date()  // 4
    val pattern = "dd/MM/yyyy HH:mm:ss"  // 5
    val dateString = formatDateToString(date, pattern)  // 6
    println(dateString)  // 7
}
```

דוגמא פלט:
```
31/12/2023 15:45:27
```

1. פונקציה שממירה תאריך למחרוזת לפי תבנית.
2. יוצרים מעצב תאריכים עם התבנית והאזור הברירת המחדל.
3. מחזירים את התאריך מעוצב.
4. יוצרים אובייקט תאריך חדש עם התאריך והשעה הנוכחית.
5. הגדרת התבנית למחרוזת - יום/חודש/שנה שעה:דקה:שנייה.
6. מחילים את הפונקציה על התאריך עם התבנית.
7. הדפסת המחרוזת.

## ניתוח מעמיק
בהיסטוריה, פורמטים רבים נוצרו להמרת תאריכים מבלי סטנדרט מוסכם, דבר שהוליד מערכות עם פורמטים שונים. `SimpleDateFormat` הוא חלק מ-Java מאז גרסא 1.1 והוא משמש עדיין ב-Kotlin עבור המרות תאריך פשוטות. ישנם גם אלטרנטיבות כגון `DateTimeFormatter` מהחבילה `java.time`, המספקת עבודה עם תאריכים בצורה יותר גמישה ובטוחה. החבילה `java.time` היא מודולית יותר וסולידית מאשר `SimpleDateFormat`, היות והיא תומכת ב-immutability ו-thread safety.

רכיבים נוספים כגון `Joda-Time` היו פופולריים עד שהופיעו ב-Java 8, אך כיום המלצה היא לעבוד עם `java.time` במידה ואין מניעה ספציפית.

במיוחד בפיתוח אנדרואיד, עשוי להיות צורך להתייחס ל-API level של ההתקן כאשר בוחרים את הספריה לניהול תאריכים.

## ראו גם
- [Class SimpleDateFormat](https://developer.android.com/reference/java/text/SimpleDateFormat)
- [Package java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [DateTimeFormatter JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Android Date and Time overview](https://developer.android.com/topic/libraries/architecture/room#java)
