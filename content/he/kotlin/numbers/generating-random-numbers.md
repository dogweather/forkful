---
title:                "גילוי מספרים אקראיים"
aliases:
- /he/kotlin/generating-random-numbers/
date:                  2024-01-27T20:35:04.999677-07:00
model:                 gpt-4-0125-preview
simple_title:         "גילוי מספרים אקראיים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?

יצירת מספרים אקראיים בתכנות מדוברת על יצירת מספרים שאינם מציגים תבנית צפויה. תכנתים עושים זאת מסיבות שונות, כולל סימולציות, בדיקות אלגוריתמים, משחקים ויישומי אבטחה, שבהם חסרון צפי הוא המפתח להשגת תוצאות ריאליסטיות או בטוחות.

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
