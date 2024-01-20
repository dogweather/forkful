---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה ולמה?
השוואה בין שני תאריכים שזה נתון חשוב שמאפשר לנו להבין את החלל זמן בין שני אירועים. למובנים אלו אנו שמים לב בהקשר של תוכניות תיזומים, מערכות ניהול, אפליקציות שיתוף פעולה, ועוד.

## כיצד לעשות:
להלן דוגמאות של קודים מוגדרים של השוואה בין שני תאריכים באמצעות Kotlin:

```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2022, 3, 20)
    val date2 = LocalDate.of(2022, 3, 21)

    println(date1.isBefore(date2)) // returns true
    println(date1.isAfter(date2)) // returns false
    println(date1.isEqual(date2)) // returns false
}
```
הפלט שמוצג מתחת להם:

```Kotlin
true
false
false
```

## צלילה עמוקה:
לאור ההשפעה של Java על Kotlin, המידע שאנו שמים כאן הגיע אלינו דרך java.time API. הAPI המקורי לא הציע את התמיכה המלאה בתאריכים, אז נפתח פרויקט תוספת שנקרא "Joda-Time". כיום, הממשקים השלמים של java.time מתמקדים בעבודה עם תאריכים. חשוב לזכור שניתן לבדוק גם את הדרך שנכתבה בביבליות שלך: Android's TimeUtils, or the Apache Commons Lang DateUtils class.

## ראו גם:
2. [Java 8 Date-Time API](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
4. [Apache's Commons Lang DateUtils class](https://commons.apache.org/proper/commons-lang/javadocs/api-3.1/org/apache/commons/lang3/time/DateUtils.html)