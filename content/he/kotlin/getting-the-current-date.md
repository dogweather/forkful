---
title:                "קבלת התאריך הנוכחי"
html_title:           "Kotlin: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?

קבלת תאריך נוכחי הוא פשוט תהליך שבו מפעילים קוד שמחזיר תאריך ושעה נוכחיים. פרקטיקה זו חיונית לפיתוח תוכניות ואפליקציות הדורשות נתונים עדכניים ותיאום עם הזמן הנוכחי.

## איך לעשות?

כדי לקבל את התאריך והשעה הנוכחיים בקוד Kotlin, ישנם מספר אפשרויות מתאימות עבור כל מטרה:

### - שימוש במשתנה ```currentDateTime```:

```Kotlin
val currentDateTime = LocalDateTime.now()
println(currentDateTime)
```
### - שימוש בפונקציה ```now()```:

```Kotlin
val currentDate = LocalDate.now()
val currentTime = LocalTime.now()
println(currentDate)
println(currentTime)
```

פלט המשתנים ```currentDate``` ו-```currentTime``` יהיו תאריך ושעה נוכחיים בהתאמה.

## העומק שבתוך

### - היסטוריית המימוש:

קבלת התאריך נוכחי בתכנות נוצרה בשנת 1970 עם תקן Unix, על ידי שימוש במערכת היחסים של הטבלה הזמנית שהיא מילי שנייה אחת מהתאריך והשעה 1970.

### - אלטרנטיבות:

ישנם גם תוכניות נוספות שקיימות בתחום שמאפשרות להשתמש בשירותי API כמו Google Time Zone API ומאפשרים לקבל תאריך נוכחי באיזורים שונים.

### - פרטים טכניים ביצועיים:

הפקודות המדויקות שמשולבות כדי לקבל תאריך נוכחי בתוך לסירת הזמן השמיים משפיעות על ביצועי הקוד הכוללים כמו נכס גישה, טעינה ושימוש משוחזרים.

## ראה גם

https://www.baeldung.com/kotlin/get-current-date-time 
https://developer.android.com/reference/java/time/LocalDateTime 
https://www.programiz.com/kotlin-programming/examples/current-datetime