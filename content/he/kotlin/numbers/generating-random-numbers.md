---
date: 2024-01-27 20:35:04.999677-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Kotlin \u05DE\u05E1\
  \u05E4\u05E7\u05EA \u05D3\u05E8\u05DA \u05D9\u05E9\u05D9\u05E8\u05D4 \u05DC\u05D9\
  \u05E6\u05D9\u05E8\u05EA \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\u05E7\u05E8\
  \u05D0\u05D9\u05D9\u05DD \u05D3\u05E8\u05DA \u05D4\u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA \u05E9\u05DC\u05D4\
  . \u05D4\u05E0\u05D4 \u05D0\u05D9\u05DA \u05D0\u05E4\u05E9\u05E8 \u05DC\u05D9\u05E6\
  \u05D5\u05E8 \u05E1\u05D5\u05D2\u05D9\u05DD \u05E9\u05D5\u05E0\u05D9\u05DD \u05E9\
  \u05DC \u05E2\u05E8\u05DB\u05D9\u05DD \u05D0\u05E7\u05E8\u05D0\u05D9\u05D9\u05DD\
  : #."
lastmod: '2024-03-13T22:44:39.264892-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u05DE\u05E1\u05E4\u05E7\u05EA \u05D3\u05E8\u05DA \u05D9\u05E9\u05D9\
  \u05E8\u05D4 \u05DC\u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05D0\u05E7\u05E8\u05D0\u05D9\u05D9\u05DD \u05D3\u05E8\u05DA \u05D4\u05E1\
  \u05E4\u05E8\u05D9\u05D9\u05D4 \u05D4\u05E1\u05D8\u05E0\u05D3\u05E8\u05D8\u05D9\u05EA\
  \ \u05E9\u05DC\u05D4."
title: "\u05D2\u05D9\u05DC\u05D5\u05D9 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\
  \u05E7\u05E8\u05D0\u05D9\u05D9\u05DD"
weight: 12
---

## איך לעשות:
Kotlin מספקת דרך ישירה ליצירת מספרים אקראיים דרך הספרייה הסטנדרטית שלה. הנה איך אפשר ליצור סוגים שונים של ערכים אקראיים:

### יצירת מספר שלם אקראי
כדי ליצור מספר שלם אקראי בתוך טווח מסוים:

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100) // יוצר מספר אקראי בין 1 ל-99
    println(randomNumber)
}
```

### יצירת Double אקראי
באופן דומה, ליצור double אקראי:

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // יוצר double אקראי בין 1.0 ל-10.0
    println(randomDouble)
}
```

### יצירת Boolean אקראי
ליצירת ערך boolean אקראי:

```kotlin
import kotlin.random.Random

fun main() {
    val randomBoolean = Random.nextBoolean() // יוצר או אמת או שקר באופן אקראי
    println(randomBoolean)
}
```

### זריעה לתוצאות ניתנות לשחזור
במקרים שבהם דרושות רצפי מספרים אקראיים ניתנים לשחזור (למשל, בבדיקות), ניתן לזרוע את יוצר המספרים האקראיים:

```kotlin
import kotlin.random.Random

fun main() {
    val seed = 12345L
    val random = Random(seed)
    val randomNumber = random.nextInt(1, 100)
    println(randomNumber)
}
```

## טבילה עמוקה
הגישה של הספרייה הסטנדרטית של Kotlin ליצירת מספרים אקראיים מנצלת את `java.util.Random` של Java מאחורי הקלעים, ומבטיחה שילוב של נוחות שימוש וביצועים. עם זאת, חשוב לציין ששיטות אלה יוצרות מספרים פסבדו-אקראיים, כלומר המספרים נראים אקראיים אך נוצרים באמצעות תהליך דטרמיניסטי.

לרוב היישומים, האקראיות שמספקת המחלקה `Random` של Kotlin מספיקה. עם זאת, ליישומים רגישים במיוחד לאבטחה, כמו קריפטוגרפיה, שבהם איכות האקראיות היא של עליונה, כדאי לשקול שימוש ב-`java.security.SecureRandom` במקום. SecureRandom מתוכנן במיוחד לפעולות קריפטוגרפיות, ומספק איכות גבוהה יותר של אקראיות, אך עם פשרה אפשרית בביצועים.

Kotlin לא ממציאה את הגלגל מחדש, אלא מציעה ממשק קוטליני ידידותי על גבי מנגנוני יצירת המספרים האקראיים של Java, ומאפשרת שימוש יותר אידיומטי ותמציתי בפרויקטים של Kotlin. כמו תמיד, כשמתמודדים עם אקראיות, תכנתים צריכים לשקול בזהירות את מקרה השימוש כדי לבחור את הכלי הרלוונטי ביותר למשימה.
